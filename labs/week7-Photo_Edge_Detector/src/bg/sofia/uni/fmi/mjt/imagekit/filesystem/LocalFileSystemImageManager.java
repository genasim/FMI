package bg.sofia.uni.fmi.mjt.imagekit.filesystem;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.List;

public class LocalFileSystemImageManager implements FileSystemImageManager{
    @Override
    public BufferedImage loadImage(File imageFile) throws IOException {
        return null;
    }

    @Override
    public List<BufferedImage> loadImagesFromDirectory(File imagesDirectory) throws IOException {
        return List.of();
    }

    @Override
    public void saveImage(BufferedImage image, File imageFile) throws IOException {

    }
}
