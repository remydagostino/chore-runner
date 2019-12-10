# Chore Tracker

Building an app for a parent to manage chores for a child.

## Url Scheme

`/chores`
`/chores/{chore_id}/edit`
`/chores/{chore_id}/stats`
`/chores/{chore_id}/stats/{user_id}`

`/chore-attempts`
`/chore-attempts/{attempt_id}`
`/chore-attempts/{attempt_id}/{step_number}`

`/login/{token}`

## Data

`User` (id, name, role)
`Token` (user_id, code)

`Chore` (id, name, reward, steps [( name, duration, bonuses)])
`ChoreAttempt` (id, chore, timestamp, log, status)

## TODO

- [ ] Ability to step through chore tasks
- [ ] Display remaining time
- [ ] Make it look nice
- [ ] Proper url navigation
- [ ] Integrate with remote data source