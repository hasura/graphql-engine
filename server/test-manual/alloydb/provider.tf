locals {
  region = "us-central1"
}

provider "google" {
  region = local.region
}

# Your name, which will prefix all resources so we know who left instances lying around.
variable "name" {
  type = string
}

data "google_project" "project" {
  provider = google-beta
}
