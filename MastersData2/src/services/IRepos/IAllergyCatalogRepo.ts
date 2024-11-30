import { Repo } from "../../core/infra/Repo";

import { AllergyCatalogItem } from "../../domain/allergyCatalogItem";
import { AllergyCatalogItemId } from "../../domain/allergyCatalogId";

export default interface IAllergyCatalogRepo extends Repo<AllergyCatalogItem> {
    save(allergy: AllergyCatalogItem): Promise<AllergyCatalogItem>;
    findByAllergyName(allergyName: string): Promise<AllergyCatalogItem>;
    findAll(): Promise<AllergyCatalogItem[]>;
    findById (id: string| AllergyCatalogItemId): Promise<AllergyCatalogItem>;
}
