terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 6.25.0"
    }
  }

  required_version = ">= 1.2"
}

provider "aws" {
  region  = "eu-central-1" # REQUIRED
  profile = "default"
}

data "aws_ami" "ubuntu" {
  most_recent = true

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd-gp3/ubuntu-noble-24.04-amd64-server-*"]
  }

  owners = ["099720109477"] # Canonical
}

resource "aws_instance" "app_server" {
  ami                         = data.aws_ami.ubuntu.id
  instance_type               = "t3.micro"
  subnet_id                   = module.vpc.public_subnets[0]
  key_name                    = "fmi-terraform-demo"
  associate_public_ip_address = true
  security_groups             = [aws_security_group.ec2_sg.id]

  tags = {
    Name = "learn-terraform"
  }
}


resource "aws_security_group" "ec2_sg" {
  name        = "fmi-terraform-db-sg"
  description = "Allow MySQL traffic"
  vpc_id      = module.vpc.vpc_id

  ingress {
    description = "Security group for SSH access to EC2 instances"
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    description = "Allow all outbound traffic"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name = "terraform-fmi-demo-db-sg"
  }
}
