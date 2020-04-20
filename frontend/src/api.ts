import { getAuthToken } from "@/auth"
import type { User, GameView } from "@/types"
import { UserV, GameViewV } from "@/types"
import type { Either } from 'fp-ts/lib/Either'
import * as t from 'io-ts'

export function authorizedFetch(url: string, mobj?: any): Promise<any> {
  const token = getAuthToken();

  if (token) {
    const obj = mobj ? mobj : {}
    const existingHeaders = obj.headers ? obj.headers : {}
    const headers = {
          Authorization: `Bearer ${token}`, ... existingHeaders }

    return fetch(url, {headers, ...obj})
  }

  throw new Error("User is unauthorized.")
}

export async function fetchMe(): Promise<Either<t.Errors, User>> {
  const response = await authorizedFetch(`http://localhost:8000/users/me`)
  const data = await response.json();
  if (!response.ok) {
    throw new Error(JSON.stringify(data))
  }
  return UserV.decode(data)
}

export async function fetchGames(userId: number): Promise<Either<t.Errors, Array<GameView>>> {
  const response = await authorizedFetch( `http://localhost:8000/users/${userId}/games`)
  const data = await response.json();
  if (!response.ok) {
    throw new Error(JSON.stringify(data));
  }
  return t.array(GameViewV).decode(data)
};

export async function fetchGame(gameId: number): Promise<Either<t.Errors, GameView>> {
  const response = await authorizedFetch( `http://localhost:8000/games/${gameId}`)
  const data = await response.json();
  if (!response.ok) {
    throw new Error(JSON.stringify(data));
  }
  return GameViewV.decode(data)
};

export async function submitCommand(gameId: number, username: string, command: string): Promise<Either<t.Errors, GameView>> {
  const response = await authorizedFetch( `http://localhost:8000/games/${gameId}/players/${username}/commands`, {method: 'POST',  body: {command }})
  const data = await response.json();
  if (!response.ok) {
    throw new Error(JSON.stringify(data));
  }
  return GameViewV.decode(data)
};
