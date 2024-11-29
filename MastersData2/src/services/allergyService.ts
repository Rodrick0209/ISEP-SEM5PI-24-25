import { Service, Inject } from 'typedi';
import config from "../../config";
import IAllergyCathalogItemDTO from '../dto/IAllergyCatalogItemDTO';
import { AllergyCathalogItem } from "../domain/allergyCathalogItem";
import { Result } from "../core/logic/Result";
import { AllergyMap } from '../mappers/AllergyMap';
import IAllergyService from './IServices/IAllergyService';
import IAllergyCatalogRepo from './IRepos/IAllergyCatalogRepo';

@Service()
export default class AllergyService implements IAllergyService {
    constructor(
        @Inject(config.repos.allergy.name) private allergyRepo : IAllergyCatalogRepo
    ) {}

    public async createAllergy(allergyDTO: IAllergyCathalogItemDTO): Promise<Result<IAllergyCathalogItemDTO>> {
        try {
            const allergyOrError = await AllergyCathalogItem.create(allergyDTO);

            if (allergyOrError.isFailure) {
                return Result.fail<IAllergyCathalogItemDTO>(allergyOrError.errorValue());
            }

            const allergyResult = allergyOrError.getValue();

            await this.allergyRepo.save(allergyResult);

            const allergyDTOResult = AllergyMap.toDTO(allergyResult) as IAllergyCathalogItemDTO;
            return Result.ok<IAllergyCathalogItemDTO>(allergyDTOResult);
        } catch (e) {
            throw e;
        }
    }


    public async listAllergies(): Promise<Result<IAllergyCathalogItemDTO[]>> {
        try {
            const allAllergies = await this.allergyRepo.findAll();

            if (allAllergies === null || allAllergies.length === 0) {
                return Result.fail<IAllergyCathalogItemDTO[]>("No allergies found");
            }

            const allergiesDTO =  allAllergies.map(allergy => AllergyMap.toDTO(allergy) as IAllergyCathalogItemDTO);
            
            return Result.ok<IAllergyCathalogItemDTO[]>(allergiesDTO);
        } catch (e) {
            throw e;
        }
    }

}