import { MedicalConditionCatalog} from "../../domain/medicalConditionCatalog";
import { Repo } from "../../core/infra/Repo";
import { MedicalConditionCatalogId } from "../../domain/medicalConditionCatalogId";
import { MedicalCondition } from "../../domain/medicalCondition";


export default interface IMedicalConditionRepo extends Repo<MedicalConditionCatalog> {
    save(medicalRecord: MedicalConditionCatalog): Promise<MedicalConditionCatalog>;
    findAll(): Promise<MedicalConditionCatalog[]>;
    findByMedicalConditionName(medicalCondition: string): Promise<MedicalConditionCatalog>;
    findByDomainId (medicalConditionId: MedicalCondition | string | MedicalConditionCatalog): Promise<MedicalConditionCatalog>;


}