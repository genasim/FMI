import { IdType, Identifiable } from "./Identifiable";

export interface RecipeDTO {
    userId: IdType;
    name: string;
    userName: string;
    shortDescription: string;
    cookingTime: number;
    ingredients: string[];
    imageUrl: string;
    description: string;
    tags: string[];
    shareDatetime: Date;
    lastModDatetime: Date;
}

export interface Recipe extends RecipeDTO, Identifiable {}