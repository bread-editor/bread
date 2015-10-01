package communication

import ()

type Hook struct {
	// Hook name
	Name string

	// Functions to be run
	Listeners []func()
}

// Run the functions in the hook
func (a *Hook) Do() {
	for _, f := range a.Listeners {
		f()
	}
}
