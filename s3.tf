resource "aws_s3_bucket" "example" {
  bucket = "my-tf-test-bucket-fmi-devops-demo"

  tags = {
    Name        = "My bucket"
    Environment = "Dev"
  }
}