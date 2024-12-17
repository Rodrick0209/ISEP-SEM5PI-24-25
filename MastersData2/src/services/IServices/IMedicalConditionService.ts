import { Result } from "../../core/logic/Result";
import IMedicalConditionCatalogDTO from "../../dto/IMedicalConditionCatalogDTO";
import IMedicalConditionDTO from "../../dto/IMedicalConditionCatalogDTO";


export default interface IMedicalConditionService {
    createMedicalCondition(medicalConditionDTO: IMedicalConditionCatalogDTO): Promise<Result<IMedicalConditionCatalogDTO>>;
    listMedicalConditions(): Promise<Result<IMedicalConditionCatalogDTO[]>>;
    getMedicalCondition(code: string): Promise<Result<IMedicalConditionCatalogDTO>>;
    updateMedicalCondition(code: string, medicalConditionCatalog: IMedicalConditionCatalogDTO): Promise<Result<IMedicalConditionCatalogDTO>>;  
}