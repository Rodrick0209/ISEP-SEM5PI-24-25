import { MedicalCondition
 } from "../../domain/medicalCondition";
import { Repo } from "../../core/infra/Repo";
import { MedicalConditionId } from "../../domain/medicalConditionId";


export default interface IMedicalConditionRepo extends Repo<MedicalCondition> {
    save(medicalRecord: MedicalCondition): Promise<MedicalCondition>;
    findAll(): Promise<MedicalCondition[]>;
}