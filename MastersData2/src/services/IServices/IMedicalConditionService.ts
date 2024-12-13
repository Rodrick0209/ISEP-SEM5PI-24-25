import { Result } from "../../core/logic/Result";
import IMedicalConditionDTO from "../../dto/IMedicalConditionCatalogDTO";


export default interface IMedicalConditionService {
    createMedicalCondition(medicalConditionDTO: IMedicalConditionDTO): Promise<Result<IMedicalConditionDTO>>;
    listMedicalConditions(): Promise<Result<IMedicalConditionDTO[]>>;
    getMedicalCondition(name: string): Promise<Result<IMedicalConditionDTO>>;
    updateMedicalCondition(name: string, nameToEdit: string): Promise<Result<IMedicalConditionDTO>>;  
}