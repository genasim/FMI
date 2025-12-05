javac -d out $(find src -name "*.java")

mkdir -p "dist"
# 2) Create a manifest file describing the main class
echo "Main-Class: bg.sofia.uni.fmi.mjt.imagekit.Main" > ./dist/manifest.txt

# 3) Package everything into a runnable jar
jar cfm ./dist/imagekit.jar ./dist/manifest.txt -C out .