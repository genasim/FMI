package bg.sofia.uni.fmi.mjt.imagekit.algorithm;

import bg.sofia.uni.fmi.mjt.imagekit.algorithm.detection.EdgeDetectionAlgorithm;
import bg.sofia.uni.fmi.mjt.imagekit.algorithm.grayscale.GrayscaleAlgorithm;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ImageAlgorithmTest {
    @Test
    void testCreateFactoryNullAlgorithmTypeThrows() {
        assertThrows(IllegalArgumentException.class, () -> ImageAlgorithm.create(null),
                     "Should throw IllegalArgumentException when trying to create ImageAlgorithm with null algorithm type");
    }

    @Test
    void testCreateFactoryGrayscale() {
        ImageAlgorithm imageAlgorithm = ImageAlgorithm.create(AlgorithmType.GRAYSCALE);
        assertInstanceOf(GrayscaleAlgorithm.class, imageAlgorithm);
    }

    @Test
    void testCreateFactoryEdgeDetection() {
        ImageAlgorithm imageAlgorithm = ImageAlgorithm.create(AlgorithmType.EDGES);
        assertInstanceOf(EdgeDetectionAlgorithm.class, imageAlgorithm);
    }

}
