package bg.sofia.uni.fmi.mjt.imagekit.filesystem;

import bg.sofia.uni.fmi.mjt.imagekit.algorithm.ImageFormat;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class LocalFileSystemImageManager implements FileSystemImageManager {
    @Override
    public BufferedImage loadImage(File imageFile) throws IOException {
        if (imageFile == null) {
            throw new IllegalArgumentException("Cannot load null image");
        }

        if (!imageFile.exists()) {
            throw new FileNotFoundException("File at " + imageFile.getAbsolutePath() + " does not exist");
        }

        if (!imageFile.isFile()) {
            throw new IOException("File at " + imageFile.getAbsolutePath() + " is not a file");
        }

        if (!isSupportedFormat(imageFile)) {
            throw new IOException(
                "File at " + imageFile.getAbsolutePath() + " is not one of the supported formats: " +
                    Arrays.toString(ImageFormat.values()));
        }
        return null;
    }

    @Override
    public List<BufferedImage> loadImagesFromDirectory(File imagesDirectory) throws IOException {
        return List.of();
    }

    @Override
    public void saveImage(BufferedImage image, File imageFile) throws IOException {

    }

    private boolean isSupportedFormat(File imageFile) {
        String fileName = imageFile.getName();
        for (ImageFormat format : ImageFormat.values()) {
            if (fileName.endsWith(format.getExtension())) {
                return true;
            }
        }
        return false;
    }
}
