package fsm

import (
	"fmt"
	"sync"

	"github.com/pkg/errors"
)

var ErrEventRejected = errors.New("event rejected")

const (
	Default StateType = ""
	NoOp    EventType = "NoOp"
)

type StateType string
type EventType string
type EventContext interface{}

type Action interface {
	Execute(eventCtx EventContext) EventType
}

type Events map[EventType]StateType

type State struct {
	Action Action
	Events Events
}

type States map[StateType]State

type StateMachine struct {
	Previous StateType
	Current  StateType
	States   States
	mutex    sync.Mutex
}

func (s *StateMachine) getNextState(event EventType) (StateType, error) {
	if state, ok := s.States[s.Current]; ok {
		if state.Events != nil {
			if next, ok := state.Events[event]; ok {
				return next, nil
			}
		}
	}

	return Default, fmt.Errorf("next state: %w: %s", ErrEventRejected, event)
}

func (s *StateMachine) SendEvent(event EventType, eventCtx EventContext) error {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	for {
		nextState, err := s.getNextState(event)
		if err != nil {
			return fmt.Errorf("%w: %s", err, event)
		}
		state, ok := s.States[nextState]
		if !ok || state.Action == nil {
			return fmt.Errorf("config error")
		}
		s.Previous = s.Current
		s.Current = nextState

		nextEvent := state.Action.Execute(eventCtx)
		if nextEvent == NoOp {
			return nil
		}
		event = nextEvent
	}
}
