# The AlloyDB cluster, with a single instance.

# The initial user password must be provided.
variable "password" {
  type      = string
  sensitive = true
}

resource "google_alloydb_cluster" "default" {
  provider   = google-beta
  cluster_id = "${var.name}-testing-alloydb"
  network    = "projects/${data.google_project.project.number}/global/networks/${google_compute_network.default.name}"
  location   = local.region

  initial_user {
    user     = var.name
    password = var.password
  }
}

resource "google_alloydb_instance" "primary" {
  provider      = google-beta
  cluster       = google_alloydb_cluster.default.name
  instance_id   = "${var.name}-testing-alloydb-instance"
  instance_type = "PRIMARY"
  depends_on = [
    google_service_networking_connection.default
  ]
}

output "url" {
  value     = "postgresql://${var.name}:${var.password}@${google_compute_instance.bastion.network_interface.0.access_config.0.nat_ip}/postgres"
  sensitive = true
}
