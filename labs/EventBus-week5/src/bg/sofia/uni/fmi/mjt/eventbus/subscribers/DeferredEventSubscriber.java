package bg.sofia.uni.fmi.mjt.eventbus.subscribers;

import bg.sofia.uni.fmi.mjt.eventbus.events.Event;
import bg.sofia.uni.fmi.mjt.eventbus.events.comparators.EventPriorityComparator;
import bg.sofia.uni.fmi.mjt.eventbus.events.comparators.EventTimestampComparator;

import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;

public class DeferredEventSubscriber<T extends Event<?>> implements Subscriber<T>, Iterable<T> {
    private final SortedSet<T> unprocessedEvents =
        new TreeSet<>(new EventPriorityComparator().thenComparing(new EventTimestampComparator()));

    /**
     * Store an event for processing at a later time.
     *
     * @param event the event to be processed
     * @throws IllegalArgumentException if the event is null
     */
    @Override
    public void onEvent(T event) {
        if (event == null) {
            throw new IllegalArgumentException("Event cannot be null");
        }
        unprocessedEvents.add(event);
    }

    /**
     * Get an iterator for the unprocessed events. The iterator should provide the events sorted
     * by priority, with higher-priority events first (lower priority number = higher priority).
     * For events with equal priority, earlier events (by timestamp) come first.
     *
     * @return an iterator for the unprocessed events
     */
    @Override
    public Iterator<T> iterator() {
        return unprocessedEvents.iterator();
    }

    /**
     * Check if there are unprocessed events.
     *
     * @return true if there are no unprocessed events, false otherwise
     */
    public boolean isEmpty() {
        return unprocessedEvents.isEmpty();
    }

}