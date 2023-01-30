# The service account is used by the bastion instance.
# It grants the relevant privileges for connecting to AlloyDB as a client.

resource "google_service_account" "service_account" {
  account_id   = "${var.name}-testing-alloydb"
  display_name = "Testing AlloyDB for ${var.name}"
}

resource "google_project_iam_binding" "service_account_alloydb_client" {
  project = data.google_project.project.id
  role    = "roles/alloydb.client"

  members = [
    google_service_account.service_account.member,
  ]
}
