-- "password" bcrypted = $2y$10$4fLjiqiJ.Rh0F/qYfIfCeOy7a9nLJN49YzUEJbYnj2ZsiwVhGsOxK
INSERT INTO public.user
  (name, email, password) 
VALUES
  ('Person', 'user@site.com', '$2y$10$4fLjiqiJ.Rh0F/qYfIfCeOy7a9nLJN49YzUEJbYnj2ZsiwVhGsOxK');

INSERT INTO public.site_admin
  (name, email, password)
VALUES
  ('Admin', 'admin@site.com', '$2y$10$4fLjiqiJ.Rh0F/qYfIfCeOy7a9nLJN49YzUEJbYnj2ZsiwVhGsOxK');
