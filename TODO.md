# TODO

- [ ] Configure customer-specific Git identities when adopting
  `~/Projects/work/<customer>/`. Keep the personal identity as the global
  default, and select each work identity using directory-based `includeIf`
  rules. Keep identity files with the corresponding customer directory and
  keep the customer-specific include rules private if needed. Verify the
  selected identity in personal repositories, each customer's repositories,
  and any linked worktrees. Do not change the global email to a work address.
