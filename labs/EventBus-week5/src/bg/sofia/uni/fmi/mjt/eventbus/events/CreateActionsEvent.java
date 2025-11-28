package bg.sofia.uni.fmi.mjt.eventbus.events;

import java.util.List;

public class CreateActionsEvent extends BaseEvent {
    // List of Integers for the sake of having some object
    public CreateActionsEvent(List<Integer> actions, String source, int priority) {
        super(new IntPayload(actions.size()), source, priority);
    }
}
