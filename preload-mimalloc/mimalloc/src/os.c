/* ----------------------------------------------------------------------------
Copyright (c) 2018-2023, Microsoft Research, Daan Leijen
This is free software; you can redistribute it and/or modify it under the
terms of the MIT license. A copy of the license can be found in the file
"LICENSE" at the root of this distribution.
-----------------------------------------------------------------------------*/
#include "mimalloc.h"
#include "mimalloc/internal.h"
#include "mimalloc/atomic.h"
#include "mimalloc/prim.h"


/* -----------------------------------------------------------
  Initialization.
  On windows initializes support for aligned allocation and
  large OS pages (if MIMALLOC_LARGE_OS_PAGES is true).
----------------------------------------------------------- */

static mi_os_mem_config_t mi_os_mem_config = {
  4096,   // page size
  0,      // large page size (usually 2MiB)
  4096,   // allocation granularity
  true,   // has overcommit?  (if true we use MAP_NORESERVE on mmap systems)
  false   // must free whole? (on mmap systems we can free anywhere in a mapped range, but on Windows we must free the entire span)
};

bool _mi_os_has_overcommit(void) {
  return mi_os_mem_config.has_overcommit;
}

// OS (small) page size
size_t _mi_os_page_size(void) {
  return mi_os_mem_config.page_size;
}

// if large OS pages are supported (2 or 4MiB), then return the size, otherwise return the small page size (4KiB)
size_t _mi_os_large_page_size(void) {
  return (mi_os_mem_config.large_page_size != 0 ? mi_os_mem_config.large_page_size : _mi_os_page_size());
}

bool _mi_os_use_large_page(size_t size, size_t alignment) {
  // if we have access, check the size and alignment requirements
  if (mi_os_mem_config.large_page_size == 0 || !mi_option_is_enabled(mi_option_large_os_pages)) return false;
  return ((size % mi_os_mem_config.large_page_size) == 0 && (alignment % mi_os_mem_config.large_page_size) == 0);
}

// round to a good OS allocation size (bounded by max 12.5% waste)
size_t _mi_os_good_alloc_size(size_t size) {
  size_t align_size;
  if (size < 512*MI_KiB) align_size = _mi_os_page_size();
  else if (size < 2*MI_MiB) align_size = 64*MI_KiB;
  else if (size < 8*MI_MiB) align_size = 256*MI_KiB;
  else if (size < 32*MI_MiB) align_size = 1*MI_MiB;
  else align_size = 4*MI_MiB;
  if mi_unlikely(size >= (SIZE_MAX - align_size)) return size; // possible overflow?
  return _mi_align_up(size, align_size);
}

void _mi_os_init(void) {
  _mi_prim_mem_init(&mi_os_mem_config);
}


/* -----------------------------------------------------------
  Util
-------------------------------------------------------------- */
bool _mi_os_decommit(void* addr, size_t size, mi_stats_t* stats);
bool _mi_os_commit(void* addr, size_t size, bool* is_zero, mi_stats_t* tld_stats);

static void* mi_align_up_ptr(void* p, size_t alignment) {
  return (void*)_mi_align_up((uintptr_t)p, alignment);
}

static void* mi_align_down_ptr(void* p, size_t alignment) {
  return (void*)_mi_align_down((uintptr_t)p, alignment);
}


/* -----------------------------------------------------------
  aligned hinting
-------------------------------------------------------------- */

// On 64-bit systems, we can do efficient aligned allocation by using
// the 2TiB to 30TiB area to allocate those.
#if (MI_INTPTR_SIZE >= 8)
static mi_decl_cache_align _Atomic(uintptr_t)aligned_base;

// Return a MI_SEGMENT_SIZE aligned address that is probably available.
// If this returns NULL, the OS will determine the address but on some OS's that may not be
// properly aligned which can be more costly as it needs to be adjusted afterwards.
// For a size > 1GiB this always returns NULL in order to guarantee good ASLR randomization;
// (otherwise an initial large allocation of say 2TiB has a 50% chance to include (known) addresses
//  in the middle of the 2TiB - 6TiB address range (see issue #372))

#define MI_HINT_BASE ((uintptr_t)2 << 40)  // 2TiB start
#define MI_HINT_AREA ((uintptr_t)4 << 40)  // upto 6TiB   (since before win8 there is "only" 8TiB available to processes)
#define MI_HINT_MAX  ((uintptr_t)30 << 40) // wrap after 30TiB (area after 32TiB is used for huge OS pages)

void* _mi_os_get_aligned_hint(size_t try_alignment, size_t size)
{
  if (try_alignment <= 1 || try_alignment > MI_SEGMENT_SIZE) return NULL;
  size = _mi_align_up(size, MI_SEGMENT_SIZE);
  if (size > 1*MI_GiB) return NULL;  // guarantee the chance of fixed valid address is at most 1/(MI_HINT_AREA / 1<<30) = 1/4096.
  #if (MI_SECURE>0)
  size += MI_SEGMENT_SIZE;        // put in `MI_SEGMENT_SIZE` virtual gaps between hinted blocks; this splits VLA's but increases guarded areas.
  #endif

  uintptr_t hint = mi_atomic_add_acq_rel(&aligned_base, size);
  if (hint == 0 || hint > MI_HINT_MAX) {   // wrap or initialize
    uintptr_t init = MI_HINT_BASE;
    #if (MI_SECURE>0 || MI_DEBUG==0)       // security: randomize start of aligned allocations unless in debug mode
    uintptr_t r = _mi_heap_random_next(mi_prim_get_default_heap());
    init = init + ((MI_SEGMENT_SIZE * ((r>>17) & 0xFFFFF)) % MI_HINT_AREA);  // (randomly 20 bits)*4MiB == 0 to 4TiB
    #endif
    uintptr_t expected = hint + size;
    mi_atomic_cas_strong_acq_rel(&aligned_base, &expected, init);
    hint = mi_atomic_add_acq_rel(&aligned_base, size); // this may still give 0 or > MI_HINT_MAX but that is ok, it is a hint after all
  }
  if (hint%try_alignment != 0) return NULL;
  return (void*)hint;
}
#else
void* _mi_os_get_aligned_hint(size_t try_alignment, size_t size) {
  MI_UNUSED(try_alignment); MI_UNUSED(size);
  return NULL;
}
#endif


/* -----------------------------------------------------------
  Free memory
-------------------------------------------------------------- */

static void mi_os_mem_free(void* addr, size_t size, bool was_committed, mi_stats_t* tld_stats) {
  MI_UNUSED(tld_stats);
  mi_assert_internal((size % _mi_os_page_size()) == 0);
  if (addr == NULL || size == 0) return; // || _mi_os_is_huge_reserved(addr)
  int err = _mi_prim_free(addr, size);
  if (err != 0) {
    _mi_warning_message("unable to free OS memory (error: %d (0x%x), size: 0x%zx bytes, address: %p)\n", err, err, size, addr);
  }
  mi_stats_t* stats = &_mi_stats_main;
  if (was_committed) { _mi_stat_decrease(&stats->committed, size); }
  _mi_stat_decrease(&stats->reserved, size);
}


void _mi_os_free_ex(void* addr, size_t size, bool was_committed, mi_stats_t* tld_stats) {
  const size_t csize = _mi_os_good_alloc_size(size);
  mi_os_mem_free(addr,csize,was_committed,tld_stats);
}

void  _mi_os_free(void* p, size_t size, mi_stats_t* tld_stats) {
  _mi_os_free_ex(p, size, true, tld_stats);
}


/* -----------------------------------------------------------
   Primitive allocation from the OS.
-------------------------------------------------------------- */

// Note: the `try_alignment` is just a hint and the returned pointer is not guaranteed to be aligned.
static void* mi_os_mem_alloc(size_t size, size_t try_alignment, bool commit, bool allow_large, bool* is_large, mi_stats_t* stats) {
  mi_assert_internal(size > 0 && (size % _mi_os_page_size()) == 0);
  if (size == 0) return NULL;
  if (!commit) allow_large = false;
  if (try_alignment == 0) try_alignment = 1; // avoid 0 to ensure there will be no divide by zero when aligning

  void* p = NULL; 
  int err = _mi_prim_alloc(size, try_alignment, commit, allow_large, is_large, &p);
  if (err != 0) {
    _mi_warning_message("unable to allocate OS memory (error: %d (0x%x), size: 0x%zx bytes, align: 0x%zx, commit: %d, allow large: %d)\n", err, err, size, try_alignment, commit, allow_large);
  }
  /*
  if (commit && allow_large) {
    p = _mi_os_try_alloc_from_huge_reserved(size, try_alignment);
    if (p != NULL) {
      *is_large = true;
      return p;
    }
  }
  */

  mi_stat_counter_increase(stats->mmap_calls, 1);
  if (p != NULL) {
    _mi_stat_increase(&stats->reserved, size);
    if (commit) { _mi_stat_increase(&stats->committed, size); }
  }
  return p;
}


// Primitive aligned allocation from the OS.
// This function guarantees the allocated memory is aligned.
static void* mi_os_mem_alloc_aligned(size_t size, size_t alignment, bool commit, bool allow_large, bool* is_large, mi_stats_t* stats) {
  mi_assert_internal(alignment >= _mi_os_page_size() && ((alignment & (alignment - 1)) == 0));
  mi_assert_internal(size > 0 && (size % _mi_os_page_size()) == 0);
  mi_assert_internal(is_large != NULL);
  if (!commit) allow_large = false;
  if (!(alignment >= _mi_os_page_size() && ((alignment & (alignment - 1)) == 0))) return NULL;
  size = _mi_align_up(size, _mi_os_page_size());

  // try first with a hint (this will be aligned directly on Win 10+ or BSD)
  void* p = mi_os_mem_alloc(size, alignment, commit, allow_large, is_large, stats);
  if (p == NULL) return NULL;

  // if not aligned, free it, overallocate, and unmap around it
  if (((uintptr_t)p % alignment != 0)) {
    mi_os_mem_free(p, size, commit, stats);
    _mi_warning_message("unable to allocate aligned OS memory directly, fall back to over-allocation (size: 0x%zx bytes, address: %p, alignment: 0x%zx, commit: %d)\n", size, p, alignment, commit);
    if (size >= (SIZE_MAX - alignment)) return NULL; // overflow
    const size_t over_size = size + alignment;

    if (mi_os_mem_config.must_free_whole) {  // win32 virtualAlloc cannot free parts of an allocate block
      // over-allocate uncommitted (virtual) memory
      p = mi_os_mem_alloc(over_size, 0 /*alignment*/, false /* commit? */, false /* allow_large */, is_large, stats);
      if (p == NULL) return NULL;

      // set p to the aligned part in the full region
      // note: this is dangerous on Windows as VirtualFree needs the actual region pointer
      // but in mi_os_mem_free we handle this (hopefully exceptional) situation.
      p = mi_align_up_ptr(p, alignment);

      // explicitly commit only the aligned part
      if (commit) {
        _mi_os_commit(p, size, NULL, stats);
      }
    }
    else  { // mmap can free inside an allocation
      // overallocate...
      p = mi_os_mem_alloc(over_size, 1, commit, false, is_large, stats);
      if (p == NULL) return NULL;
      // and selectively unmap parts around the over-allocated area. (noop on sbrk)
      void* aligned_p = mi_align_up_ptr(p, alignment);
      size_t pre_size = (uint8_t*)aligned_p - (uint8_t*)p;
      size_t mid_size = _mi_align_up(size, _mi_os_page_size());
      size_t post_size = over_size - pre_size - mid_size;
      mi_assert_internal(pre_size < over_size&& post_size < over_size&& mid_size >= size);
      if (pre_size > 0)  mi_os_mem_free(p, pre_size, commit, stats);
      if (post_size > 0) mi_os_mem_free((uint8_t*)aligned_p + mid_size, post_size, commit, stats);
      // we can return the aligned pointer on `mmap` (and sbrk) systems
      p = aligned_p;
    }
  }

  mi_assert_internal(p == NULL || (p != NULL && ((uintptr_t)p % alignment) == 0));
  return p;
}


/* -----------------------------------------------------------
  OS API: alloc and alloc_aligned
----------------------------------------------------------- */

void* _mi_os_alloc(size_t size, mi_stats_t* tld_stats) {
  MI_UNUSED(tld_stats);
  mi_stats_t* stats = &_mi_stats_main;
  if (size == 0) return NULL;
  size = _mi_os_good_alloc_size(size);
  bool is_large = false;
  return mi_os_mem_alloc(size, 0, true, false, &is_large, stats);
}

void* _mi_os_alloc_aligned(size_t size, size_t alignment, bool commit, bool* large, mi_stats_t* tld_stats)
{
  MI_UNUSED(&_mi_os_get_aligned_hint); // suppress unused warnings
  MI_UNUSED(tld_stats);
  if (size == 0) return NULL;
  size = _mi_os_good_alloc_size(size);
  alignment = _mi_align_up(alignment, _mi_os_page_size());
  bool allow_large = false;
  if (large != NULL) {
    allow_large = *large;
    *large = false;
  }
  return mi_os_mem_alloc_aligned(size, alignment, commit, allow_large, (large!=NULL?large:&allow_large), &_mi_stats_main /*tld->stats*/ );
}

/* -----------------------------------------------------------
  OS aligned allocation with an offset. This is used
  for large alignments > MI_ALIGNMENT_MAX. We use a large mimalloc
  page where the object can be aligned at an offset from the start of the segment.
  As we may need to overallocate, we need to free such pointers using `mi_free_aligned`
  to use the actual start of the memory region.
----------------------------------------------------------- */

void* _mi_os_alloc_aligned_offset(size_t size, size_t alignment, size_t offset, bool commit, bool* large, mi_stats_t* tld_stats) {
  mi_assert(offset <= MI_SEGMENT_SIZE);
  mi_assert(offset <= size);
  mi_assert((alignment % _mi_os_page_size()) == 0);
  if (offset > MI_SEGMENT_SIZE) return NULL;
  if (offset == 0) {
    // regular aligned allocation
    return _mi_os_alloc_aligned(size, alignment, commit, large, tld_stats);
  }
  else {
    // overallocate to align at an offset
    const size_t extra = _mi_align_up(offset, alignment) - offset;
    const size_t oversize = size + extra;
    void* start = _mi_os_alloc_aligned(oversize, alignment, commit, large, tld_stats);
    if (start == NULL) return NULL;
    void* p = (uint8_t*)start + extra;
    mi_assert(_mi_is_aligned((uint8_t*)p + offset, alignment));
    // decommit the overallocation at the start
    if (commit && extra > _mi_os_page_size()) {
      _mi_os_decommit(start, extra, tld_stats);
    }
    return p;
  }
}

void _mi_os_free_aligned(void* p, size_t size, size_t alignment, size_t align_offset, bool was_committed, mi_stats_t* tld_stats) {
  mi_assert(align_offset <= MI_SEGMENT_SIZE);
  const size_t extra = _mi_align_up(align_offset, alignment) - align_offset;
  void* start = (uint8_t*)p - extra;
  _mi_os_free_ex(start, size + extra, was_committed, tld_stats);
}


/* -----------------------------------------------------------
  OS memory API: reset, commit, decommit, protect, unprotect.
----------------------------------------------------------- */

// OS page align within a given area, either conservative (pages inside the area only),
// or not (straddling pages outside the area is possible)
static void* mi_os_page_align_areax(bool conservative, void* addr, size_t size, size_t* newsize) {
  mi_assert(addr != NULL && size > 0);
  if (newsize != NULL) *newsize = 0;
  if (size == 0 || addr == NULL) return NULL;

  // page align conservatively within the range
  void* start = (conservative ? mi_align_up_ptr(addr, _mi_os_page_size())
    : mi_align_down_ptr(addr, _mi_os_page_size()));
  void* end = (conservative ? mi_align_down_ptr((uint8_t*)addr + size, _mi_os_page_size())
    : mi_align_up_ptr((uint8_t*)addr + size, _mi_os_page_size()));
  ptrdiff_t diff = (uint8_t*)end - (uint8_t*)start;
  if (diff <= 0) return NULL;

  mi_assert_internal((conservative && (size_t)diff <= size) || (!conservative && (size_t)diff >= size));
  if (newsize != NULL) *newsize = (size_t)diff;
  return start;
}

static void* mi_os_page_align_area_conservative(void* addr, size_t size, size_t* newsize) {
  return mi_os_page_align_areax(true, addr, size, newsize);
}

// Commit/Decommit memory.
// Usually commit is aligned liberal, while decommit is aligned conservative.
// (but not for the reset version where we want commit to be conservative as well)
static bool mi_os_commitx(void* addr, size_t size, bool commit, bool conservative, bool* is_zero, mi_stats_t* stats) {
  // page align in the range, commit liberally, decommit conservative
  if (is_zero != NULL) { *is_zero = false; }
  size_t csize;
  void* start = mi_os_page_align_areax(conservative, addr, size, &csize);
  if (csize == 0) return true;  // || _mi_os_is_huge_reserved(addr))
  if (commit) {
    _mi_stat_increase(&stats->committed, size);  // use size for precise commit vs. decommit
    _mi_stat_counter_increase(&stats->commit_calls, 1);
  }
  else {
    _mi_stat_decrease(&stats->committed, size);
  }

  int err = _mi_prim_commit(start, csize, commit);  
  if (err != 0) {
    _mi_warning_message("cannot %s OS memory (error: %d (0x%x), address: %p, size: 0x%zx bytes)\n", commit ? "commit" : "decommit", err, err, start, csize);
  }
  mi_assert_internal(err == 0);
  return (err == 0);
}

bool _mi_os_commit(void* addr, size_t size, bool* is_zero, mi_stats_t* tld_stats) {
  MI_UNUSED(tld_stats);
  mi_stats_t* stats = &_mi_stats_main;
  return mi_os_commitx(addr, size, true, false /* liberal */, is_zero, stats);
}

bool _mi_os_decommit(void* addr, size_t size, mi_stats_t* tld_stats) {
  MI_UNUSED(tld_stats);
  mi_stats_t* stats = &_mi_stats_main;
  bool is_zero;
  return mi_os_commitx(addr, size, false, true /* conservative */, &is_zero, stats);
}

/*
static bool mi_os_commit_unreset(void* addr, size_t size, bool* is_zero, mi_stats_t* stats) {  
  return mi_os_commitx(addr, size, true, true // conservative
                      , is_zero, stats);
}
*/

// Signal to the OS that the address range is no longer in use
// but may be used later again. This will release physical memory
// pages and reduce swapping while keeping the memory committed.
// We page align to a conservative area inside the range to reset.
static bool mi_os_resetx(void* addr, size_t size, bool reset, mi_stats_t* stats) {
  // page align conservatively within the range
  size_t csize;
  void* start = mi_os_page_align_area_conservative(addr, size, &csize);
  if (csize == 0) return true;  // || _mi_os_is_huge_reserved(addr)
  if (reset) _mi_stat_increase(&stats->reset, csize);
        else _mi_stat_decrease(&stats->reset, csize);
  if (!reset) return true; // nothing to do on unreset!

  #if (MI_DEBUG>1) && !MI_SECURE && !MI_TRACK_ENABLED // && !MI_TSAN
  memset(start, 0, csize); // pretend it is eagerly reset
  #endif

  int err = _mi_prim_reset(start, csize);
  if (err != 0) {
    _mi_warning_message("cannot reset OS memory (error: %d (0x%x), address: %p, size: 0x%zx bytes)\n", err, err, start, csize);
  }
  return (err == 0);
}

// Signal to the OS that the address range is no longer in use
// but may be used later again. This will release physical memory
// pages and reduce swapping while keeping the memory committed.
// We page align to a conservative area inside the range to reset.
bool _mi_os_reset(void* addr, size_t size, mi_stats_t* tld_stats) {
  MI_UNUSED(tld_stats);
  mi_stats_t* stats = &_mi_stats_main;
  return mi_os_resetx(addr, size, true, stats);
}

/*
bool _mi_os_unreset(void* addr, size_t size, bool* is_zero, mi_stats_t* tld_stats) {
  MI_UNUSED(tld_stats);
  mi_stats_t* stats = &_mi_stats_main;
  *is_zero = false;
  return mi_os_resetx(addr, size, false, stats);
}
*/

// Protect a region in memory to be not accessible.
static  bool mi_os_protectx(void* addr, size_t size, bool protect) {
  // page align conservatively within the range
  size_t csize = 0;
  void* start = mi_os_page_align_area_conservative(addr, size, &csize);
  if (csize == 0) return false;
  /*
  if (_mi_os_is_huge_reserved(addr)) {
	  _mi_warning_message("cannot mprotect memory allocated in huge OS pages\n");
  }
  */
  int err = _mi_prim_protect(start,csize,protect);
  if (err != 0) {
    _mi_warning_message("cannot %s OS memory (error: %d (0x%x), address: %p, size: 0x%zx bytes)\n", (protect ? "protect" : "unprotect"), err, err, start, csize);
  }
  return (err == 0);
}

bool _mi_os_protect(void* addr, size_t size) {
  return mi_os_protectx(addr, size, true);
}

bool _mi_os_unprotect(void* addr, size_t size) {
  return mi_os_protectx(addr, size, false);
}



/* ----------------------------------------------------------------------------
Support for allocating huge OS pages (1Gib) that are reserved up-front
and possibly associated with a specific NUMA node. (use `numa_node>=0`)
-----------------------------------------------------------------------------*/
#define MI_HUGE_OS_PAGE_SIZE  (MI_GiB)


#if (MI_INTPTR_SIZE >= 8)
// To ensure proper alignment, use our own area for huge OS pages
static mi_decl_cache_align _Atomic(uintptr_t)  mi_huge_start; // = 0

// Claim an aligned address range for huge pages
static uint8_t* mi_os_claim_huge_pages(size_t pages, size_t* total_size) {
  if (total_size != NULL) *total_size = 0;
  const size_t size = pages * MI_HUGE_OS_PAGE_SIZE;

  uintptr_t start = 0;
  uintptr_t end = 0;
  uintptr_t huge_start = mi_atomic_load_relaxed(&mi_huge_start);
  do {
    start = huge_start;
    if (start == 0) {
      // Initialize the start address after the 32TiB area
      start = ((uintptr_t)32 << 40);  // 32TiB virtual start address
    #if (MI_SECURE>0 || MI_DEBUG==0)      // security: randomize start of huge pages unless in debug mode
      uintptr_t r = _mi_heap_random_next(mi_prim_get_default_heap());
      start = start + ((uintptr_t)MI_HUGE_OS_PAGE_SIZE * ((r>>17) & 0x0FFF));  // (randomly 12bits)*1GiB == between 0 to 4TiB
    #endif
    }
    end = start + size;
    mi_assert_internal(end % MI_SEGMENT_SIZE == 0);
  } while (!mi_atomic_cas_strong_acq_rel(&mi_huge_start, &huge_start, end));

  if (total_size != NULL) *total_size = size;
  return (uint8_t*)start;
}
#else
static uint8_t* mi_os_claim_huge_pages(size_t pages, size_t* total_size) {
  MI_UNUSED(pages);
  if (total_size != NULL) *total_size = 0;
  return NULL;
}
#endif

// Allocate MI_SEGMENT_SIZE aligned huge pages
void* _mi_os_alloc_huge_os_pages(size_t pages, int numa_node, mi_msecs_t max_msecs, size_t* pages_reserved, size_t* psize) {
  if (psize != NULL) *psize = 0;
  if (pages_reserved != NULL) *pages_reserved = 0;
  size_t size = 0;
  uint8_t* start = mi_os_claim_huge_pages(pages, &size);
  if (start == NULL) return NULL; // or 32-bit systems

  // Allocate one page at the time but try to place them contiguously
  // We allocate one page at the time to be able to abort if it takes too long
  // or to at least allocate as many as available on the system.
  mi_msecs_t start_t = _mi_clock_start();
  size_t page = 0;
  while (page < pages) {
    // allocate a page
    void* addr = start + (page * MI_HUGE_OS_PAGE_SIZE);
    void* p = NULL;
    int err = _mi_prim_alloc_huge_os_pages(addr, MI_HUGE_OS_PAGE_SIZE, numa_node, &p);
    if (err != 0) {
      _mi_warning_message("unable to allocate huge OS page (error: %d (0x%x), address: %p, size: %zx bytes)\n", err, err, addr, MI_HUGE_OS_PAGE_SIZE);
      break;
    }

    // Did we succeed at a contiguous address?
    if (p != addr) {
      // no success, issue a warning and break
      if (p != NULL) {
        _mi_warning_message("could not allocate contiguous huge OS page %zu at %p\n", page, addr);
        _mi_os_free(p, MI_HUGE_OS_PAGE_SIZE, &_mi_stats_main);
      }
      break;
    }

    // success, record it
    page++;  // increase before timeout check (see issue #711)
    _mi_stat_increase(&_mi_stats_main.committed, MI_HUGE_OS_PAGE_SIZE);
    _mi_stat_increase(&_mi_stats_main.reserved, MI_HUGE_OS_PAGE_SIZE);

    // check for timeout
    if (max_msecs > 0) {
      mi_msecs_t elapsed = _mi_clock_end(start_t);
      if (page >= 1) {
        mi_msecs_t estimate = ((elapsed / (page+1)) * pages);
        if (estimate > 2*max_msecs) { // seems like we are going to timeout, break
          elapsed = max_msecs + 1;
        }
      }
      if (elapsed > max_msecs) {
        _mi_warning_message("huge OS page allocation timed out (after allocating %zu page(s))\n", page);
        break;
      }
    }
  }
  mi_assert_internal(page*MI_HUGE_OS_PAGE_SIZE <= size);
  if (pages_reserved != NULL) { *pages_reserved = page; }
  if (psize != NULL) { *psize = page * MI_HUGE_OS_PAGE_SIZE; }
  return (page == 0 ? NULL : start);
}

// free every huge page in a range individually (as we allocated per page)
// note: needed with VirtualAlloc but could potentially be done in one go on mmap'd systems.
void _mi_os_free_huge_pages(void* p, size_t size, mi_stats_t* stats) {
  if (p==NULL || size==0) return;
  uint8_t* base = (uint8_t*)p;
  while (size >= MI_HUGE_OS_PAGE_SIZE) {
    _mi_os_free(base, MI_HUGE_OS_PAGE_SIZE, stats);
    size -= MI_HUGE_OS_PAGE_SIZE;
    base += MI_HUGE_OS_PAGE_SIZE;
  }
}

/* ----------------------------------------------------------------------------
Support NUMA aware allocation
-----------------------------------------------------------------------------*/

_Atomic(size_t)  _mi_numa_node_count; // = 0   // cache the node count

size_t _mi_os_numa_node_count_get(void) {
  size_t count = mi_atomic_load_acquire(&_mi_numa_node_count);
  if (count <= 0) {
    long ncount = mi_option_get(mi_option_use_numa_nodes); // given explicitly?
    if (ncount > 0) {
      count = (size_t)ncount;
    }
    else {
      count = _mi_prim_numa_node_count(); // or detect dynamically
      if (count == 0) count = 1;
    }
    mi_atomic_store_release(&_mi_numa_node_count, count); // save it
    _mi_verbose_message("using %zd numa regions\n", count);
  }
  return count;
}

int _mi_os_numa_node_get(mi_os_tld_t* tld) {
  MI_UNUSED(tld);
  size_t numa_count = _mi_os_numa_node_count();
  if (numa_count<=1) return 0; // optimize on single numa node systems: always node 0
  // never more than the node count and >= 0
  size_t numa_node = _mi_prim_numa_node();
  if (numa_node >= numa_count) { numa_node = numa_node % numa_count; }
  return (int)numa_node;
}
