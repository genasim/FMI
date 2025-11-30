package bg.sofia.uni.fmi.mjt.pipeline.stage;

import bg.sofia.uni.fmi.mjt.pipeline.step.Step;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.description;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

class StageTest {
    private Stage<?, ?> stage;

    @BeforeEach
    void setUp() {
        Step<?, ?> initialStep = mock(Step.class);
        stage = Stage.start(initialStep);
    }

    @Test
    void testStartWithNullInitialStageThrows() {
        assertThrows(IllegalArgumentException.class, () -> Stage.start(null),
                     "Should throw IllegalArgumentException when starting with null initial stage");
    }

    @Test
    void testAddStepAddingNullStepThrows() {
        assertThrows(IllegalArgumentException.class, () -> stage.addStep(null),
                     "Should throw IllegalArgumentException when adding null step");
    }

    @Test
    void testStartWithInitialStepHasOnlyOneStep() {
        Step<?, ?> initialStep = mock(Step.class);
        var stage = Stage.start(initialStep);

        assertEquals(1, stage.getSteps().size(), "Stage should have only one step");
    }

    @Test
    <I, O> void testAddStepAddingStepToInitialStage() {
        @SuppressWarnings("unchecked")
        Step<I, O> initialStep = mock(Step.class);
        var stage = Stage.start(initialStep);

        @SuppressWarnings("unchecked")
        Step<? super O, ?> step = mock(Step.class);
        stage.addStep(step);

        assertEquals(2, stage.getSteps().size(), "Stage should have two steps");
        assertSame(step, stage.getSteps().get(1), "Second step should be the one that was added");
    }

    @Test
    <I, O> void testAddStepReturnsSameStageInstance() {
        @SuppressWarnings("unchecked")
        Step<I, O> initialStep = mock(Step.class);
        var stage = Stage.start(initialStep);

        @SuppressWarnings("unchecked")
        Step<? super O, ?> step = mock(Step.class);
        var modifiedStage = stage.addStep(step);

        assertSame(stage, modifiedStage, "Should return the same stage instance after adding a step");
    }

    @Test
    void testExecuteSingleStep() {
        Step<Integer, String> initialStep = mock(Step.class);
        when(initialStep.process(40)).thenReturn("40");

        Stage<Integer, String> stage = Stage.start(initialStep);
        String output = stage.execute(40);

        assertEquals("40", output);
        verify(initialStep, description("Should process data just once")).process(40);
    }

    @Test
    void testExecuteMultipleSteps() {
        Step<Integer, Integer> initialStep = mock(Step.class);
        when(initialStep.process(4)).thenReturn(16);

        Step<Integer, String> step2 = mock(Step.class);
        when(step2.process(16)).thenReturn("16");

        Step<String, Integer> step3 = mock(Step.class);
        when(step3.process("16")).thenReturn(2); // string length

        Stage<Integer, Integer> stage = Stage.start(initialStep).addStep(step2).addStep(step3);
        Integer output = stage.execute(4);

        assertEquals(2, output);

        verify(initialStep, description("Should call process on initialStep once")).process(4);
        verify(step2, description("Should call process on step2 once")).process(16);
        verify(step3, description("Should call process on step3 once")).process("16");
        verifyNoMoreInteractions(initialStep, step2, step3);
    }
}
