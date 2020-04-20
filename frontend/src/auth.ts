export function setAuthToken(token: string) {
  localStorage.setItem("token", token);
}

export function getAuthToken(): string | null {
  return localStorage.getItem("token");
}

export function isAuthenticated(): boolean {
  return getAuthToken() !== null;
}
