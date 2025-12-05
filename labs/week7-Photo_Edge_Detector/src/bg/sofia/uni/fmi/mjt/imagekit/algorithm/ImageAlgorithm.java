package bg.sofia.uni.fmi.mjt.imagekit.algorithm;

import bg.sofia.uni.fmi.mjt.imagekit.algorithm.detection.SobelEdgeDetection;
import bg.sofia.uni.fmi.mjt.imagekit.algorithm.grayscale.LuminosityGrayscale;

import java.awt.image.BufferedImage;

/**
 * Represents an algorithm that processes images.
 */
public interface ImageAlgorithm {

    /**
     * Applies the image processing algorithm to the given image.
     *
     * @param image the image to be processed
     * @return BufferedImage the processed image of type (TYPE_INT_RGB)
     * @throws IllegalArgumentException if the image is null
     */
    BufferedImage process(BufferedImage image);

    static ImageAlgorithm create(AlgorithmType algorithmType) {
        if (algorithmType == null) {
            throw new IllegalArgumentException("Cannot create an ImageAlgorithm with a null algorithm type");
        }

        return switch (algorithmType) {
            case GRAYSCALE -> new LuminosityGrayscale();
            case EDGES -> new SobelEdgeDetection(new LuminosityGrayscale());
        };
    }
}