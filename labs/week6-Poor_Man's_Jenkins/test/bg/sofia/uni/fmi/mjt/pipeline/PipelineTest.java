package bg.sofia.uni.fmi.mjt.pipeline;

import bg.sofia.uni.fmi.mjt.pipeline.stage.Stage;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.anyDouble;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class PipelineTest {
    @Test
    void testStartNullInitialStageThrows() {
        assertThrows(IllegalArgumentException.class, () -> Pipeline.start(null),
                     "Should throw IllegalArgumentException when starting with null initial stage");
    }

    @Test
    void testStartReturnsNewPipelineInstanceWithJustTheInitialStage() {
        Stage<?, ?> initialStage = mock(Stage.class);
        var pipeline = Pipeline.start(initialStage);

        assertEquals(1, pipeline.getStages().size(), "Pipeline should have only the initial stage");
        assertSame(initialStage, pipeline.getStages().getFirst(), "Initial stage should be the one that was passed");
    }

    @Test
    void testAddStageAddNullStageSkipsIt() {
        var pipeline = new Pipeline<>(List.of(), new Cache());
        pipeline.addStage(null);

        assertEquals(0, pipeline.getStages().size(), "Pipeline should not add null stage");
    }

    @Test
    void testAddStageReturnsSamePipelineInstance() {
        Pipeline<?, ?> pipeline = Pipeline.start(mock(Stage.class));
        var result = pipeline.addStage(mock(Stage.class));

        assertSame(pipeline, result, "Should return the same pipeline instance");
    }

    @Test
    void testAddStageAddMultipleStages() {
        var stage1 = mock(Stage.class);
        var stage2 = mock(Stage.class);
        var stage3 = mock(Stage.class);
        Pipeline<?, ?> pipeline = new Pipeline(List.of(stage1, stage2, stage3), new Cache());

        assertEquals(3, pipeline.getStages().size(), "Pipeline should have 3 stages");
        assertSame(stage1, pipeline.getStages().getFirst(), "First stage should be the one that was added first");
        assertSame(stage2, pipeline.getStages().get(1), "Second stage should be the one that was added second");
        assertSame(stage3, pipeline.getStages().getLast(), "Third stage should be the one that was added last");
    }

    @Test
    void testAddStageClearsCache() {
        Cache cache = new Cache();
        cache.cacheValue(42, "test");

        Pipeline<?, ?> pipeline = new Pipeline<>(new ArrayList<>(), cache);
        pipeline.addStage(mock(Stage.class));

        assertTrue(cache.isEmpty(), "Cache should be empty after adding a stage");
        assertFalse(cache.containsKey(42), "Cache should not contain any keys after adding a stage");
        assertNull(cache.getCachedValue(42), "Cache should not contain any values after adding a stage");
    }

    @Test
    void testExecuteInputCacheHit() {
        Cache cache = mock(Cache.class);
        when(cache.containsKey(anyInt())).thenReturn(true);
        when(cache.getCachedValue(anyInt())).thenReturn(100);

        var pipeline = new Pipeline<Integer, Integer>(List.of(), cache);
        int result = pipeline.execute(42);

        assertEquals(100, result, "Pipeline should return cached value");
    }

    @Test
    void testExecuteInputCacheMissSingleStage() {
        Cache cache = mock(Cache.class);
        Stage<Double, Integer> stage = mock(Stage.class);

        when(cache.containsKey(42d)).thenReturn(false);
        when(stage.execute(42d)).thenReturn(20);

        var pipeline = new Pipeline<Double, Integer>(List.of(stage), cache);
        int result = pipeline.execute(42d);

        assertEquals(20, result, "Pipeline should execute stage and return result");
    }

    @Test
    void testExecuteInputCacheMissMultipleStages() {
        Cache cache = mock(Cache.class);
        Stage<Double, Integer> stage1 = mock(Stage.class);
        Stage<Integer, String> stage2 = mock(Stage.class);

        when(cache.containsKey(anyDouble())).thenReturn(false);
        when(stage1.execute(anyDouble())).thenReturn(42);
        when(stage2.execute(anyInt())).thenReturn("RESULT");

        var pipeline = new Pipeline<Double, String>(List.of(stage1, stage2), cache);
        String result = pipeline.execute(-1d);

        assertEquals("RESULT", result, "Pipeline should execute stage and return result");
    }
}
