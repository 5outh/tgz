import { getAuthToken } from "@/auth"

export function authorizedFetch(url: string, token: string, mobj?: any): Promise<any> {
  const obj = mobj ? mobj : {}
  const existingHeaders = obj.headers ? obj.headers : {}
  const headers = {
        Authorization: `Bearer ${token}`, ... existingHeaders }

  return fetch(url, {headers, ...obj})

}
