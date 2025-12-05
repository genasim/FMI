package bg.sofia.uni.fmi.mjt.imagekit.algorithm.detection;

import bg.sofia.uni.fmi.mjt.imagekit.algorithm.ImageAlgorithm;
import bg.sofia.uni.fmi.mjt.imagekit.algorithm.pixels.PixelUtils;

import java.awt.image.BufferedImage;

public class SobelEdgeDetection implements EdgeDetectionAlgorithm {
    // the two kernels are fixed well-known matrices;
    private static final short[][] HORIZONTAL_KERNEL = new short[][] {
        {-1, 0, 1},
        {-2, 0, 2},
        {-1, 0, 1}
    };
    private static final short[][] VERTICAL_KERNEL = new short[][] {
        {-1, -2, -1},
        {0, 0, 0},
        {1, 2, 1}
    };
    private static final int KERNEL_SIZE = 3;

    private final ImageAlgorithm grayscaleAlgorithm;

    public SobelEdgeDetection(ImageAlgorithm grayscaleAlgorithm) {
        if (grayscaleAlgorithm == null) {
            throw new IllegalArgumentException("Grayscale algorithm cannot be null");
        }
        this.grayscaleAlgorithm = grayscaleAlgorithm;
    }

    @Override
    public BufferedImage process(BufferedImage image) {
        if (image == null) {
            throw new IllegalArgumentException("Cannot process null image");
        }

        BufferedImage grayscaleImage = grayscaleAlgorithm.process(image);
        short[][] intensityMatrix = getPixelIntensityMatrix(grayscaleImage);

        BufferedImage edgesImage =
            new BufferedImage(grayscaleImage.getWidth(), grayscaleImage.getHeight(), BufferedImage.TYPE_INT_RGB);
        for (int x = 0; x < grayscaleImage.getWidth(); x++) {
            for (int y = 0; y < grayscaleImage.getHeight(); y++) {
                short[][] region = getPixelRegion(x, y, intensityMatrix);

                int horizontal = convolute(region, HORIZONTAL_KERNEL);
                int vertical = convolute(region, VERTICAL_KERNEL);

                double gradient = Math.sqrt(horizontal * horizontal + vertical * vertical);
                int rbg = PixelUtils.getRGBValue(gradient, gradient, gradient);
                edgesImage.setRGB(x, y, rbg);
            }
        }

        return edgesImage;
    }

    private static short[][] getPixelIntensityMatrix(BufferedImage image) {
        short[][] matrix = new short[image.getWidth()][image.getHeight()];
        for (int x = 0; x < image.getWidth(); x++) {
            for (int y = 0; y < image.getHeight(); y++) {
                matrix[x][y] = PixelUtils.getGrayscaleValue(image.getRGB(x, y));
            }
        }
        return matrix;
    }

    private static short[][] getPixelRegion(int x, int y, short[][] intensityMatrix) {
        int width = intensityMatrix.length;
        int height = intensityMatrix[0].length;

        short[][] region = new short[KERNEL_SIZE][KERNEL_SIZE];
        for (int i = 0; i < KERNEL_SIZE; i++) {
            for (int j = 0; j < KERNEL_SIZE; j++) {
                int ix = Math.clamp(x - 1 + i, 0, width - 1);
                int iy = Math.clamp(y - 1 + j, 0, height - 1);

                boolean isWithinBounds =
                    ix >= 0 && iy >= 0 && ix < width && iy < height;
                region[i][j] = isWithinBounds ? intensityMatrix[ix][iy] : 0;
            }
        }
        return region;
    }

    private static int convolute(short[][] matrix, short[][] kernel) {
        int convolution = 0;
        for (int i = 0; i < KERNEL_SIZE; i++) {
            for (int j = 0; j < KERNEL_SIZE; j++) {
                convolution += matrix[i][j] * kernel[i][j];
            }
        }
        return convolution;
    }
}
