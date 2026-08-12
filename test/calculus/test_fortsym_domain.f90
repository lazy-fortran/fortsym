program test_fortsym_domain
    ! Metadata owner checks. The independent oracle is the declared contract:
    ! invalid dimensions/names are refused, while patch facts are explicit and
    ! never inferred from a coordinate expression.
    use fortsym_domain
    implicit none

    type(manifold_t) :: manifold, same, other, invalid
    type(patch_t) :: patch, closed_patch, invalid_patch

    manifold = manifold_create("M", 4, has_boundary=.false., &
        simply_connected=.true.)
    same = manifold_create("M", 4)
    other = manifold_create("N", 4)
    invalid = manifold_create("", 4)
    if (.not. manifold_valid(manifold)) error stop "manifold rejected"
    if (manifold_dimension(manifold) /= 4) error stop "dimension lost"
    if (trim(manifold_name(manifold)) /= "M") error stop "name lost"
    if (manifold_has_boundary(manifold)) error stop "boundary flag failed"
    if (.not. manifold_simply_connected(manifold)) then
        error stop "simple-connectivity flag failed"
    end if
    if (.not. same_manifold(manifold, same)) error stop "same manifold failed"
    if (same_manifold(manifold, other)) error stop "different manifold matched"
    if (manifold_valid(invalid)) error stop "invalid manifold accepted"

    patch = patch_create(manifold, "U", simply_connected=.true.)
    closed_patch = patch_create(manifold, "V", open_domain=.false., &
        has_boundary=.true.)
    invalid_patch = patch_create(invalid, "bad")
    if (.not. patch_valid(patch)) error stop "patch rejected"
    if (patch_dimension(patch) /= 4) error stop "patch dimension lost"
    if (trim(patch_name(patch)) /= "U") error stop "patch name lost"
    if (.not. patch_is_open(patch)) error stop "open patch flag failed"
    if (.not. patch_simply_connected(patch)) then
        error stop "patch topology flag failed"
    end if
    if (.not. same_patch_parent(patch, manifold)) then
        error stop "patch parent failed"
    end if
    if (patch_is_open(closed_patch)) error stop "closed domain accepted"
    if (.not. patch_has_boundary(closed_patch)) then
        error stop "patch boundary flag failed"
    end if
    if (patch_valid(invalid_patch)) error stop "invalid patch accepted"

    print *, "test_fortsym_domain: all checks passed"
end program test_fortsym_domain
