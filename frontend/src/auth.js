export function setAuthToken(token: string) {
  localStorage.setItem("token", token);
}

export function getAuthToken(): ?number {
  return localStorage.getItem("token");
}
