import bg.sofia.uni.fmi.mjt.eventbus.EventBus;
import bg.sofia.uni.fmi.mjt.eventbus.EventBusImpl;
import bg.sofia.uni.fmi.mjt.eventbus.events.ActionPayload;
import bg.sofia.uni.fmi.mjt.eventbus.events.CreateActionsEvent;
import bg.sofia.uni.fmi.mjt.eventbus.events.DeleteActionEvent;
import bg.sofia.uni.fmi.mjt.eventbus.models.Action;
import bg.sofia.uni.fmi.mjt.eventbus.subscribers.Subscriber;
import bg.sofia.uni.fmi.mjt.eventbus.subscribers.SyncSubscriber;

void main() {
    EventBus bus = new EventBusImpl();

    Subscriber<CreateActionsEvent> sub1 = new SyncSubscriber<>();
    Subscriber<CreateActionsEvent> sub2 = new SyncSubscriber<>();
    Subscriber<DeleteActionEvent> sub3 = new SyncSubscriber<>();

//    Subscriber<CreateActionsEvent> deferredSub = new DeferredEventSubscriber<>();

    bus.subscribe(CreateActionsEvent.class, sub1);
//    bus.subscribe(CreateActionsEvent.class, sub2);
    bus.subscribe(DeleteActionEvent.class, sub3);
//    bus.subscribe(CreateActionsEvent.class, deferredSub);

    bus.publish(new CreateActionsEvent(List.of(1, 2, 3), "google.com", 2));

    Instant start = Instant.now();
    bus.publish(new CreateActionsEvent(List.of(), "wikipedia.com", 2));
    bus.publish(new DeleteActionEvent(new ActionPayload(new Action("Make new film")), "netflix.com", -1));
    bus.publish(new CreateActionsEvent(List.of(4, 5), "youtube.com", 4));
    Instant end = Instant.now();

    System.out.printf("Event Logs: %s", bus.getEventLogs(CreateActionsEvent.class, start, end));
}
