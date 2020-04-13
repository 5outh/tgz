import * as t from 'io-ts'

const Username = t.type({
  username: t.string
})
export type Username = t.TypeOf<typeof Username>

const PlayerInfo = t.type(
  { username : Username
  , email    : t.string
  , playerId : t.number
  })
export type PlayerInfo = t.TypeOf<typeof PlayerInfo>

const Points = t.type(
  { points : t.number
  , step : t.number
  })
export type Points = t.TypeOf<typeof Points>

// TODO: Fix
// export type Rotated<T> = { Rotated: T } | {UnRotated: T}

const Rotated = <C extends t.Mixed>(codec: C) => t.union([t.type({Rotated: codec}), t.type({UnRotated: codec}) ])

// const _Rotated = <C extends t.Mixed>(codec: C) => t.type({Rotated: codec})
// const _UnRotated = <C extends t.Mixed>(codec: C) => t.type({UnRotated: codec})
// const Rotated = <C extends t.Mixed>(codec: C) => t.union([_Rotated<C>,_UnRotated<C>])
// export type Rotated<T> = t.TypeOf<typeof Rotated<T>>

const Links = t.type({
  previous: t.string,
  next: t.string
})

interface TechnologyCardState
  { price : number
  , cattle : number
  }

export interface TechnologyCard
  { name               : string
  , craftsmanType      : Craftsman
  , victoryRequirement : number
  , victoryPoints      : number
  , cost               : number
  }

type Activation = 'BuilderActive' | 'NomadsActive' | 'None'

export interface Player
  { info               : PlayerInfo | null
  , victoryRequirement : Points
  , victoryPoints      : Points
  , empire             : Empire | null
  , cattle             : number
      , monuments          : Map<Location, number>
      , craftsmen          : Map<Location,  Rotated<Craftsman>>
      , technologyCards    : Map<TechnologyCard, TechnologyCardState>
      , specialists        : Map<Specialist,  number>
  , god                : God | null
      , activations : Array<Activation>
  }

type Empire
  = "kilwa"
  | "mutapa"
  | "zulu"
  | "lozi"
  | "mapungubwe"

type EmpirePlaque = { PlayerPlaque:  Empire } | { ShadipinyiPlaque: any }

export interface GenerosityOfKingsState
  { plaques       : Array<EmpirePlaque>
  , cattlePool    : number
  , lastBid       : number | null
  , playersPassed : Array<number>
  }

type Phase
  = "PreSetup"
  | "Setup"
  | "GenerosityOfKings"
  | "ReligionAndCulture"
  | "Revenues"
  | "LetUsCompareMythologies"

export interface Round
  { players                : Array<number>
  , currentPlayer          : number | null
  , usedMarkers            : Map<Location,  number>
  , generosityOfKingsState : GenerosityOfKingsState
  , currentPhase           : Phase | null
  , step : number
  }

type Land
  = { StartingArea : any }
  | { BlankLand : any }
  | { Resource : Resource }

export type Square
  = { Water : any }
  | { Land:  Land }

export interface MapLayout { mapLayout : Map<Location, Square> }

export type Craftsman
  = "potter"
  | "ivoryCarver"
  | "woodCarver"
  | "diamondCutter"
  | "vesselMaker"
  | "throneMaker"
  | "sculptor"

export interface TechnologyCard
  { name               : string
  , craftsmanType      : Craftsman
  , victoryRequirement : number
  , victoryPoints      : number
  , cost               : number
  }

export type God
  = { Shadipinyi: any }
  | { Elegua: any }
  | { Dziva: any }
  | { Eshu: any }
  | { Gu: any }
  | { Obatala: any }
  | { Atete: any }
  | { TsuiGoab: any }
  | { Anansi: any }
  | { Qamata: number }
  | { Engai : any }
  | { Xango  : any }

export type Specialist
  = "Shaman"
  | "RainCeremony"
  | "Herd"
  | "Builder"
  | "Nomads"

export type Resource = "Clay" | "Wood" | "Ivory" | "Diamonds"

export interface Game
  { players   : Map<number, Player>
  , round     : Round
  , mapLayout : MapLayout
  , technologyCards : Map<Craftsman, Array<TechnologyCard>>
  , gods : Array<God>
  , specialists : Array<Specialist>
  , winner    : number | null
  , step : number
  , resourceTiles : Map<Resource,  number>
  , waterTiles : number
  , craftsmanTiles : Map<Craftsman, number>
  }
