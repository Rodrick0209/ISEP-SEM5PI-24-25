import { Service, Inject } from 'typedi';
import config from "../../config";
import IMedicalConditionDTO from '../dto/IMedicalConditionDTO';
import IMedicalConditionRepo from './IRepos/IMedicalConditionRepo';
import IMedicalConditionService from './IServices/IMedicalConditionService';
import { Result } from "../core/logic/Result";
import { MedicalCondition } from "../domain/medicalCondition";
import { MedicalConditionMap } from '../mappers/MedicalConditionMap';

@Service()
export default class MedicalConditionService implements IMedicalConditionService {
    constructor(
        @Inject(config.repos.medicalCondition.name) private medicalConditionRepo : IMedicalConditionRepo
    ) {}

    public async createMedicalCondition(medicalConditionDTO: IMedicalConditionDTO): Promise<Result<IMedicalConditionDTO>> {
        try { 
            const medicalConditionOrError = await MedicalCondition.create(medicalConditionDTO);
            
            if (medicalConditionOrError.isFailure) {
                return Result.fail<IMedicalConditionDTO>(medicalConditionOrError.errorValue());
            }

            const medicalConditionResult = medicalConditionOrError.getValue();

            await this.medicalConditionRepo.save(medicalConditionResult);

            const medicalConditionDTOResult = MedicalConditionMap.toDTO(medicalConditionResult) as IMedicalConditionDTO;
            return Result.ok<IMedicalConditionDTO>(medicalConditionDTOResult);
        } catch (e) {
            throw e;
        }
    }

    public async listMedicalConditions(): Promise<Result<IMedicalConditionDTO[]>> { 
        try {
            const allMedicalConditions = await this.medicalConditionRepo.findAll();

            if (allMedicalConditions === null || allMedicalConditions.length === 0) {
                return Result.fail<IMedicalConditionDTO[]>("No medical conditions found");
            }

            const medicalConditionsDTO =  allMedicalConditions.map(medicalCondition => MedicalConditionMap.toDTO(medicalCondition) as IMedicalConditionDTO);
            
            return Result.ok<IMedicalConditionDTO[]>(medicalConditionsDTO);
        } catch (e) {
            throw e;
        }
    }




}