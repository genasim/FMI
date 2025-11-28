package bg.sofia.uni.fmi.mjt.eventbus;

import bg.sofia.uni.fmi.mjt.eventbus.events.Event;
import bg.sofia.uni.fmi.mjt.eventbus.events.comparators.EventTimestampComparator;
import bg.sofia.uni.fmi.mjt.eventbus.exception.MissingSubscriptionException;
import bg.sofia.uni.fmi.mjt.eventbus.subscribers.Subscriber;

import java.time.Instant;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

public class EventBusImpl implements EventBus {
    private final Map<Class<? extends Event<?>>, List<Subscriber<?>>> eventSubscribers = new HashMap<>();
    private final SortedSet<Event<?>> eventLogs = new TreeSet<>(new EventTimestampComparator());

    @Override
    public <T extends Event<?>> void subscribe(Class<T> eventType, Subscriber<? super T> subscriber) {
        if (eventType == null) {
            throw new IllegalArgumentException("Cannot subscribe to null eventType");
        }

        if (subscriber == null) {
            throw new IllegalArgumentException("Cannot subscribe null subscriber");
        }

        eventSubscribers.putIfAbsent(eventType, new LinkedList<>());
        List<Subscriber<?>> subscribers = eventSubscribers.get(eventType);
        if (!subscribers.contains(subscriber)) {
            subscribers.add(subscriber);
        }
    }

    @Override
    public <T extends Event<?>> void unsubscribe(Class<T> eventType, Subscriber<? super T> subscriber)
        throws MissingSubscriptionException {
        if (eventType == null) {
            throw new IllegalArgumentException("EventType cannot be null");
        }

        if (subscriber == null) {
            throw new IllegalArgumentException("Subscriber cannot be null");
        }

        List<Subscriber<?>> subscribers = eventSubscribers.get(eventType);
        if (subscribers == null || !subscribers.remove(subscriber)) {
            throw new MissingSubscriptionException(
                String.format("Could not find subscriber for event type; subscriber: %s", subscriber));
        }
    }

    @Override
    public <T extends Event<?>> void publish(T event) {
        if (event == null) {
            throw new IllegalArgumentException("Event cannot be null");
        }
        eventLogs.add(event);

        @SuppressWarnings("unchecked")
        List<Subscriber<? super T>> subscribers =
            (List<Subscriber<? super T>>) (List<?>) eventSubscribers.get(event.getClass());
        if (subscribers == null) {
            return;
        }

        for (Subscriber<? super T> subscriber : subscribers) {
            subscriber.onEvent(event);
        }
    }

    @Override
    public void clear() {
        for (List<Subscriber<?>> subscribers : eventSubscribers.values()) {
            subscribers.clear();
        }
        eventSubscribers.clear();
        eventLogs.clear();
    }

    @Override
    public Collection<? extends Event<?>> getEventLogs(Class<? extends Event<?>> eventType, Instant from, Instant to) {
        if (eventType == null) {
            throw new IllegalArgumentException("Cannot get event logs for null eventType");
        }

        if (from == null || to == null) {
            throw new IllegalArgumentException("Cannot get event logs if either timestamps is null");
        }

        if (from.equals(to)) {
            return List.of();
        }

        return List.copyOf(buildEventLogs(eventType, from, to));
    }

    @Override
    public <T extends Event<?>> Collection<Subscriber<?>> getSubscribersForEvent(Class<T> eventType) {
        if (eventType == null) {
            throw new IllegalArgumentException("Cannot get subscribers for null eventType");
        }

        return List.copyOf(eventSubscribers.getOrDefault(eventType, List.of()));
    }

    private List<Event<?>> buildEventLogs(Class<? extends Event<?>> eventType, Instant from, Instant to) {
        List<Event<?>> logs = new LinkedList<>();
        for (Event<?> event : eventLogs) {
            if (!eventType.isInstance(event)) {
                continue;
            }

            Instant timestamp = event.getTimestamp();
            if (timestamp.isBefore(from)) {
                continue;
            }

            if (!timestamp.isBefore(to)) {
                break;
            }

            logs.add(event);
        }
        return logs;
    }
}
