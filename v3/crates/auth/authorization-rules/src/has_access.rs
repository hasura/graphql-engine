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
    pub fn append(self, other: Self) -> Self {
        match (self, other) {
            // deny always stays as deny
            (HasAccess::Deny, _) => HasAccess::Deny,
            // "don't know"" is ignored
            (_, HasAccess::DontKnow) => self,
            _ => other,
        }
    }

    pub fn has_access(self) -> bool {
        matches!(self, HasAccess::Allow)
    }
}

#[test]
fn test_has_access_append() {
    // don't know is always set to new value
    assert_eq!(
        HasAccess::DontKnow.append(HasAccess::DontKnow),
        HasAccess::DontKnow
    );
    assert_eq!(
        HasAccess::DontKnow.append(HasAccess::Allow),
        HasAccess::Allow
    );
    assert_eq!(HasAccess::DontKnow.append(HasAccess::Deny), HasAccess::Deny);

    // allow is set to allow or deny
    assert_eq!(
        HasAccess::Allow.append(HasAccess::DontKnow),
        HasAccess::Allow
    );
    assert_eq!(HasAccess::Allow.append(HasAccess::Allow), HasAccess::Allow);
    assert_eq!(HasAccess::Allow.append(HasAccess::Deny), HasAccess::Deny);

    // deny always stays at deny
    assert_eq!(HasAccess::Deny.append(HasAccess::DontKnow), HasAccess::Deny);
    assert_eq!(HasAccess::Deny.append(HasAccess::Allow), HasAccess::Deny);
    assert_eq!(HasAccess::Deny.append(HasAccess::Deny), HasAccess::Deny);
}
