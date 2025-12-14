resource "aws_db_instance" "db" {
  allocated_storage      = 10
  db_name                = "FmiTerraformDemo"
  engine                 = "mysql"
  engine_version         = "8.0"
  instance_class         = "db.t3.micro"
  username               = "foo"
  password               = "foobarbaz"
  parameter_group_name   = "default.mysql8.0"
  vpc_security_group_ids = [aws_security_group.db_sg.id]
  skip_final_snapshot    = true
}

resource "aws_security_group" "db_sg" {
  name        = "fmi-terraform-db-sg"
  description = "Allow MySQL traffic"
  vpc_id      = data.aws_vpc.default_vpc.id

  ingress {
    description = "MySQL from VPC"
    from_port   = 3306
    to_port     = 3306
    protocol    = "tcp"
    cidr_blocks = [module.vpc.vpc_cidr_block]
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
