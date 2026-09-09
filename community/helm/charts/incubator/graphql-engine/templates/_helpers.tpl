{{/* vim: set filetype=mustache: */}}
{{/*
Expand the name of the chart.
*/}}
{{- define "hasura-graphql-engine.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
If release name contains chart name it will be used as a full name.
*/}}
{{- define "hasura-graphql-engine.fullname" -}}
  {{- if .Values.fullnameOverride -}}
    {{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" -}}
  {{- else -}}
    {{- $name := default .Chart.Name .Values.nameOverride -}}
    {{- if contains $name .Release.Name -}}
      {{- .Release.Name | trunc 63 | trimSuffix "-" -}}
    {{- else -}}
      {{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" -}}
    {{- end -}}
  {{- end -}}
{{- end -}}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "hasura-graphql-engine.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Common labels
*/}}
{{- define "hasura-graphql-engine.labels" -}}
helm.sh/chart: {{ include "hasura-graphql-engine.chart" . }}
{{ include "hasura-graphql-engine.selectorLabels" . }}
  {{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
  {{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end -}}

{{/*
Selector labels
*/}}
{{- define "hasura-graphql-engine.selectorLabels" -}}
app.kubernetes.io/name: {{ include "hasura-graphql-engine.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end -}}

{{/*
Create the name of the service account to use
*/}}
{{- define "hasura-graphql-engine.serviceAccountName" -}}
  {{- if .Values.serviceAccount.create -}}
    {{ default (include "hasura-graphql-engine.fullname" .) .Values.serviceAccount.name }}
  {{- else -}}
    {{ default "default" .Values.serviceAccount.name }}
  {{- end -}}
{{- end -}}

{{- define "postgres.fullname" -}}
{{- printf "%s-%s" .Release.Name "postgresql" | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{- define "hasura.databaseHost" -}}
  {{- if .Values.hasura.database.postgres.managed.enabled }}
      {{- printf "%s" (include "postgres.fullname" .) -}}
  {{- else -}}
      {{- printf "%s" (required "Database host required!" .Values.hasura.database.postgres.external.host) -}}
  {{- end -}}
{{- end -}}

{{- define "hasura.databasePort" -}}
  {{- if .Values.hasura.database.postgres.managed.enabled }}
      {{- printf "5432" -}}
  {{- else -}}
      {{- printf "%s" (required "Database port required!" .Values.hasura.database.postgres.external.port) -}}
  {{- end -}}
{{- end -}}

{{- define "hasura.databaseUser" -}}
  {{- if .Values.hasura.database.postgres.managed.enabled }}
      {{- printf "%s" .Values.hasura.database.postgres.managed.userName -}}
  {{- else -}}
      {{- printf "%s" (required "Database username required!" .Values.hasura.database.postgres.external.userName) -}}
  {{- end -}}
{{- end -}}

{{- define "hasura.databasePassword" -}}
  {{- if .Values.hasura.database.postgres.managed.enabled }}
      {{- required "impossible" nil -}}
  {{- else -}}
      {{- printf "%s" .Values.hasura.database.postgres.external.password -}}
  {{- end -}}
{{- end -}}

{{- define "hasura.databaseName" -}}
  {{- if .Values.hasura.database.postgres.managed.enabled }}
      {{- printf "%s" .Values.hasura.database.postgres.managed.containerDatabase -}}
  {{- else -}}
      {{- printf "%s" (required "Database name required!" .Values.hasura.database.postgres.external.containerDatabase) -}}
  {{- end -}}
{{- end -}}

{{- define "hasura.databaseUrl" -}}
  {{- printf "postgres://%s:%s@%s:%s/%s" (include "hasura.databaseUser" .) (include "hasura.databasePassword" .) (include "hasura.databaseHost" .) (include "hasura.databasePort" .) (include "hasura.databaseName" .) -}}
{{- end -}}
