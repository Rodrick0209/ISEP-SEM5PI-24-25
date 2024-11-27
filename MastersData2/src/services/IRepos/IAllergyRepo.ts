import { Repo } from "../../core/infra/Repo";

import { Allergy } from "../../domain/allergy";
import { AllergyId } from "../../domain/allergyId";

export default interface IAllergyRepo extends Repo<Allergy> {
    save(allergy: Allergy): Promise<Allergy>;
    findByAllergyName(allergyName: string): Promise<Allergy>;
    findAll(): Promise<Allergy[]>;
    findById (id: string| AllergyId): Promise<Allergy>;
}
