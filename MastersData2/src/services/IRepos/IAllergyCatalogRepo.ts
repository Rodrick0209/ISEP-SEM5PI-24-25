import { Repo } from "../../core/infra/Repo";

import { AllergyCatalogItem } from "../../domain/allergyCatalogItem";
import { AllergyCatalogItemId } from "../../domain/allergyCatalogId";
import { Allergy } from "../../domain/allergy";

export default interface IAllergyCatalogRepo extends Repo<AllergyCatalogItem> {
    save(allergy: AllergyCatalogItem): Promise<AllergyCatalogItem>;
    findByCode(code: string): Promise<AllergyCatalogItem>;
    findAll(): Promise<AllergyCatalogItem[]>;
    findById (id: string| AllergyCatalogItemId| Allergy): Promise<AllergyCatalogItem>;
}
