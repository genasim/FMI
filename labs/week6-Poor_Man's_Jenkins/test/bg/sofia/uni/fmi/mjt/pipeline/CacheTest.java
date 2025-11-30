package bg.sofia.uni.fmi.mjt.pipeline;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CacheTest {
    private Cache cache;

    @BeforeEach
    void setUp() {
        cache = new Cache();
    }

    @Test
    void testCacheValueNullKeyThrows() {
        assertThrows(IllegalArgumentException.class, () -> cache.cacheValue(null, "test"),
                     "Cache should throw IllegalArgumentException when caching null key");
    }

    @Test
    void testCacheValueNullValueThrows() {
        assertThrows(IllegalArgumentException.class, () -> cache.cacheValue(4, null),
                     "Cache should throw IllegalArgumentException when caching null value");
    }

    @Test
    void testCacheValueCachesValue() {
        cache.cacheValue(4, "test");

        assertEquals("test", cache.getCachedValue(4), "Cache should cache value");
        assertFalse(cache.isEmpty(), "Cache should not be empty after caching value");
    }

    @Test
    void testCacheValueOverwritesPreviousValue() {
        cache.cacheValue("a", 100);
        cache.cacheValue("b", -6);
        cache.cacheValue("a", 20);

        assertEquals(20, cache.getCachedValue("a"), "Cache should overwrite previous value");
    }

    @Test
    void testIsEmptyReturnsTrueWhenCacheIsEmpty() {
        assertTrue(cache.isEmpty(), "Cache should be empty when initialized");
    }

    @Test
    void testIsEmptyReturnsFalseWhenCacheIsNotEmpty() {
        cache.cacheValue(4, "test");
        assertFalse(cache.isEmpty(), "Cache should not be empty when it contains values");
    }

    @Test
    void testIsEmptyAfterClearingReturnsTrue() {
        cache.cacheValue(4, "test");
        cache.clear();
        assertTrue(cache.isEmpty(), "Cache should be empty after clearing");
    }

    @Test
    void testIsEmptyAfterClearingEmptyCacheReturnsTrue() {
        cache.clear();
        assertTrue(cache.isEmpty(), "Cache should be empty after clearing already empty cache");
    }

    @Test
    void testContainsKeySearchingNullKeyThrows() {
        assertThrows(IllegalArgumentException.class, () -> cache.containsKey(null),
                     "Cache should throw IllegalArgumentException when searching by null key");
    }

    @Test
    void testContainsKeyReturnsFalseForNonExistingKey() {
        assertFalse(cache.containsKey(4), "Cache should return false when searching for non-existing key");
    }

    @Test
    void testContainsKeyHasKey() {
        cache.cacheValue(4, "test");
        assertTrue(cache.containsKey(4), "Cache should return true when searching for existing key");
    }

    @Test
    void testGetCachedValueSearchingNullKeyThrows() {
        assertThrows(IllegalArgumentException.class, () -> cache.getCachedValue(null),
                     "Cache should throw IllegalArgumentException when searching by null key");
    }

    @Test
    void testGetCachedValueReturnsNullForNonExistingKey() {
        assertNull(cache.getCachedValue(4), "Cache should return null when searching for non-existing key");
    }

    @Test
    void testGetCachedValueReturnsValueForExistingKey() {
        cache.cacheValue(4, "test");
        assertEquals("test", cache.getCachedValue(4), "Cache should return value for existing key");
    }
}
