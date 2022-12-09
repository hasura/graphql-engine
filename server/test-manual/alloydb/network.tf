# AlloyDB requires a dedicated virtual private network.

resource "google_compute_network" "default" {
  name = "${var.name}-testing-alloydb-network"
}

resource "google_compute_global_address" "private_ip_alloc" {
  name          = "${var.name}-testing-alloydb-cluster"
  address_type  = "INTERNAL"
  purpose       = "VPC_PEERING"
  prefix_length = 16
  network       = google_compute_network.default.id
}

resource "google_service_networking_connection" "default" {
  network                 = google_compute_network.default.id
  service                 = "servicenetworking.googleapis.com"
  reserved_peering_ranges = [google_compute_global_address.private_ip_alloc.name]
}

# Allows instances with the tag "ssh" to expose port 22.
resource "google_compute_firewall" "ssh" {
  name = "${var.name}-testing-alloydb-allow-ssh"
  allow {
    ports    = ["22"]
    protocol = "tcp"
  }
  direction     = "INGRESS"
  network       = google_compute_network.default.id
  priority      = 1000
  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["ssh"]
}

# Allows instances with the tag "postgres" to expose port 5432.
resource "google_compute_firewall" "postgres" {
  name = "${var.name}-testing-alloydb-allow-postgres"
  allow {
    ports    = ["5432"]
    protocol = "tcp"
  }
  direction     = "INGRESS"
  network       = google_compute_network.default.id
  priority      = 1000
  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["postgres"]
}
