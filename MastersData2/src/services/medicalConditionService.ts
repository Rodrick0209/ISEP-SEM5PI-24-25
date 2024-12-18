import { Service, Inject } from 'typedi';
import config from "../../config";
import IMedicalConditionDTO from '../dto/IMedicalConditionCatalogDTO';
import IMedicalConditionRepo from './IRepos/IMedicalConditionRepo';
import IMedicalConditionService from './IServices/IMedicalConditionService';
import { Result } from "../core/logic/Result";
import { MedicalConditionCatalog } from "../domain/medicalConditionCatalog";
import { MedicalConditionCatalogMap } from '../mappers/MedicalConditionCatalogMap';
import IMedicalConditionCatalogDTO from '../dto/IMedicalConditionCatalogDTO';
import { MedicalConditionCatalogId } from '../domain/medicalConditionCatalogId';

@Service()
export default class MedicalConditionService implements IMedicalConditionService {
    constructor(
        @Inject(config.repos.medicalCondition.name) private medicalConditionRepo : IMedicalConditionRepo
    ) {}

    public async getMedicalCondition(code: string): Promise<Result<IMedicalConditionCatalogDTO>> {
        try {
            const medicalCondition = await this.medicalConditionRepo.findByCode(code);

            if (medicalCondition === null) {
                return Result.fail<IMedicalConditionCatalogDTO>("No medical condition found");
            }

            const medicalConditionDTO = MedicalConditionCatalogMap.toDTO(medicalCondition) as IMedicalConditionCatalogDTO;
            return Result.ok<IMedicalConditionCatalogDTO>(medicalConditionDTO);
        } catch (e) {
            throw e;
        }
    }
    
    public async updateMedicalCondition(code: string, medicalConditionCatalog: IMedicalConditionCatalogDTO): Promise<Result<IMedicalConditionCatalogDTO>> {
        try {
            const medicalCondition = await this.medicalConditionRepo.findByCode(code);

            if (medicalCondition === null) {
                return Result.fail<IMedicalConditionDTO>("No medical condition found");
            }

            if(medicalConditionCatalog.description != null){
                medicalCondition.designation = medicalConditionCatalog.designation;
            }

            if(medicalConditionCatalog.designation != null){
                medicalCondition.description = medicalConditionCatalog.description;
            }

            await this.medicalConditionRepo.save(medicalCondition);

            const medicalConditionDTO = MedicalConditionCatalogMap.toDTO(medicalCondition) as IMedicalConditionDTO;
            return Result.ok<IMedicalConditionDTO>(medicalConditionDTO);
        } catch (e) {
            throw e;
        }
    }

    public async createMedicalCondition(medicalConditionDTO: IMedicalConditionCatalogDTO): Promise<Result<IMedicalConditionCatalogDTO>> {
        try { 
            const medicalConditionOrError = await MedicalConditionCatalog.create(medicalConditionDTO);
            
            if (medicalConditionOrError.isFailure) {
                return Result.fail<IMedicalConditionDTO>(medicalConditionOrError.errorValue());
            }

            const medicalConditionResult = medicalConditionOrError.getValue();

            await this.medicalConditionRepo.save(medicalConditionResult);

            const medicalConditionDTOResult = MedicalConditionCatalogMap.toDTO(medicalConditionResult as MedicalConditionCatalog) as IMedicalConditionDTO;
            return Result.ok<IMedicalConditionDTO>(medicalConditionDTOResult);
        } catch (e) {
            throw e;
        }
    }

    public async listMedicalConditions(): Promise<Result<IMedicalConditionCatalogDTO[]>> { 
        try {
            const allMedicalConditions = await this.medicalConditionRepo.findAll();

            if (allMedicalConditions === null || allMedicalConditions.length === 0) {
                return Result.fail<IMedicalConditionCatalogDTO[]>("No medical conditions found");
            }

            const medicalConditionsDTO =  allMedicalConditions.map(medicalCondition => MedicalConditionCatalogMap.toDTO(medicalCondition) as IMedicalConditionCatalogDTO);
            
            return Result.ok<IMedicalConditionCatalogDTO[]>(medicalConditionsDTO);
        } catch (e) {
            throw e;
        }
    }




}