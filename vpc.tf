module "vpc" {
  source = "terraform-aws-modules/vpc/aws"

  name = "fmi-terraform-vpc"
  cidr = "10.0.0.0/16"

  azs             = ["eu-central-1a", "eu-central-1b", "eu-central-1c"]
  private_subnets = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
  public_subnets  = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]

  enable_nat_gateway = false
  enable_vpn_gateway = false

  tags = {
    Terraform   = "true"
    Environment = "dev"
  }
}

data "aws_vpc" "default_vpc" {
  default = true
}

resource "aws_vpc_peering_connection" "foo" {
  peer_vpc_id = data.aws_vpc.default_vpc.id
  vpc_id      = module.vpc.vpc_id

  auto_accept = true
}

locals {
  route_tables = concat(
    module.vpc.public_route_table_ids,
    module.vpc.private_route_table_ids,
    [module.vpc.default_route_table_id]
  )
}

resource "aws_route" "to_default_vpc" {
  for_each = toset(local.route_tables)

  route_table_id            = each.value
  destination_cidr_block    = data.aws_vpc.default_vpc.cidr_block
  vpc_peering_connection_id = aws_vpc_peering_connection.foo.id
}
