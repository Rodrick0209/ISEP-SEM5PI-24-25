import { Repo } from "../../core/infra/Repo";

import { AllergyCathalogItem } from "../../domain/allergyCathalogItem";
import { AllergyCathalogItemId } from "../../domain/allergyCathalogId";

export default interface IAllergyCatalogRepo extends Repo<AllergyCathalogItem> {
    save(allergy: AllergyCathalogItem): Promise<AllergyCathalogItem>;
    findByAllergyName(allergyName: string): Promise<AllergyCathalogItem>;
    findAll(): Promise<AllergyCathalogItem[]>;
    findById (id: string| AllergyCathalogItemId): Promise<AllergyCathalogItem>;
}
