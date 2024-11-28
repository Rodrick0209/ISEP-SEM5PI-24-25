import { Service, Inject } from 'typedi';
import config from "../../config";
import IAllergyDTO from '../dto/IAllergyDTO';
import { Allergy } from "../domain/allergy";
import { Result } from "../core/logic/Result";
import { AllergyMap } from '../mappers/AllergyMap';
import IAllergyService from './IServices/IAllergyService';
import IAllergyRepo from './IRepos/IAllergyRepo';

@Service()
export default class AllergyService implements IAllergyService {
    constructor(
        @Inject(config.repos.allergy.name) private allergyRepo : IAllergyRepo
    ) {}

    public async createAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>> {
        try {
            const allergyOrError = await Allergy.create(allergyDTO);

            if (allergyOrError.isFailure) {
                return Result.fail<IAllergyDTO>(allergyOrError.errorValue());
            }

            const allergyResult = allergyOrError.getValue();

            await this.allergyRepo.save(allergyResult);

            const allergyDTOResult = AllergyMap.toDTO(allergyResult) as IAllergyDTO;
            return Result.ok<IAllergyDTO>(allergyDTOResult);
        } catch (e) {
            throw e;
        }
    }


    public async listAllergies(): Promise<Result<IAllergyDTO[]>> {
        try {
            const allAllergies = await this.allergyRepo.findAll();

            if (allAllergies === null || allAllergies.length === 0) {
                return Result.fail<IAllergyDTO[]>("No allergies found");
            }

            const allergiesDTO =  allAllergies.map(allergy => AllergyMap.toDTO(allergy) as IAllergyDTO);
            
            return Result.ok<IAllergyDTO[]>(allergiesDTO);
        } catch (e) {
            throw e;
        }
    }

}