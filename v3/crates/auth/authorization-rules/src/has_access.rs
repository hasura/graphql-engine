use metadata_resolve::AllowOrDeny;

#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub enum HasAccess {
    #[default]
    DontKnow,
    Allow,
    Deny,
}

// we have a sort of (semi-?) lattice thing going on here
// the values only flow in one direction from `DontKnow` to `Allow` to `Deny`
impl HasAccess {
    pub fn set(&mut self, other: &AllowOrDeny) {
        *self = match (&self, other) {
            // deny always stays as deny
            (HasAccess::Deny, _) | (_, AllowOrDeny::Deny) => HasAccess::Deny,
            (_, AllowOrDeny::Allow) => HasAccess::Allow,
        };
    }

    pub fn has_access(self) -> bool {
        matches!(self, HasAccess::Allow)
    }
}

#[test]
fn test_has_access_set_dont_know() {
    // don't know is always set to new value
    let mut one = HasAccess::DontKnow;
    one.set(&AllowOrDeny::Allow);
    assert_eq!(one, HasAccess::Allow);

    let mut two = HasAccess::DontKnow;
    two.set(&AllowOrDeny::Deny);
    assert_eq!(two, HasAccess::Deny);
}

#[test]
fn test_has_access_set_allow() {
    // allow is set to allow or deny
    let mut one = HasAccess::Allow;
    one.set(&AllowOrDeny::Allow);
    assert_eq!(one, HasAccess::Allow);

    let mut two = HasAccess::Allow;
    two.set(&AllowOrDeny::Deny);
    assert_eq!(two, HasAccess::Deny);
}

#[test]
fn test_has_access_set_deny() {
    // deny always stays at deny
    let mut one = HasAccess::Deny;
    one.set(&AllowOrDeny::Allow);
    assert_eq!(one, HasAccess::Deny);

    let mut two = HasAccess::Deny;
    two.set(&AllowOrDeny::Deny);
    assert_eq!(two, HasAccess::Deny);
}
