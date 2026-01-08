resource "aws_lb" "test" {
  name               = "test-lb-tf"
  internal           = false
  load_balancer_type = "application" bun create bun create elysia app  elysia app 
  security_groups    = [aws_security_group.allow_public-access_to_lb.id]
  subnets            = module.vpc.public_subnets

  enable_deletion_protection = false

#   access_logs {
#     bucket  = aws_s3_bucket.lb_logs.id
#     prefix  = "test-lb"
#     enabled = true
#   }

  tags = {
    Environment = "production"
  }
}

resource "aws_security_group" "allow_public-access_to_lb" {
  name        = "allow_tls"
  description = "Allow TLS public access to load balancer"
  vpc_id      = module.vpc.vpc_id

  tags = {
    Name = "allow_tls_lb"
  }
}