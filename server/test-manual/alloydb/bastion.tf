# The bastion instance, which runs the AlloyDB auth proxy.

# This grabs the name of the latest Debian image.
data "google_compute_image" "bastion_image" {
  family  = "debian-11"
  project = "debian-cloud"
}

resource "google_compute_instance" "bastion" {
  name         = "${var.name}-testing-alloydb-bastion"
  machine_type = "e2-small"
  zone         = "us-central1-a"
  tags = [
    # "ssh", # uncomment and re-apply to SSH in
    "postgres",
  ]

  network_interface {
    network = google_compute_network.default.id

    # Runs on an ephemeral public IP address.
    access_config {}
  }

  boot_disk {
    initialize_params {
      image = data.google_compute_image.bastion_image.self_link
    }
  }

  # This service account has client access to AlloyDB.
  service_account {
    email  = google_service_account.service_account.email
    scopes = ["cloud-platform"]
  }

  # On startup, download the AlloyDB auth proxy and run it.
  # Logs are written to /alloydb-auth-proxy.log. You can SSH in and view them if necessary.
  metadata_startup_script = <<-EOT
    #!/usr/bin/env bash

    set -e
    set -u
    set -o pipefail

    curl -fsSL https://storage.googleapis.com/alloydb-auth-proxy/v0.6.1/alloydb-auth-proxy.linux.amd64 -o alloydb-auth-proxy
    chmod +x alloydb-auth-proxy

    nohup ./alloydb-auth-proxy \
      '${google_alloydb_instance.primary.id}' \
      --address "0.0.0.0" \
      >& alloydb-auth-proxy.log &
  EOT
}
