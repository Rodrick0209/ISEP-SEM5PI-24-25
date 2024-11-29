import { Result } from "../../core/logic/Result";
import IAllergyCathalogItemDTO from "../../dto/IAllergyCatalogItemDTO";


export default interface IAllergyService {
    createAllergy(allergyDTO: IAllergyCathalogItemDTO): Promise<Result<IAllergyCathalogItemDTO>>;
    listAllergies(): Promise<Result<IAllergyCathalogItemDTO[]>>;
    //updateAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>>;
    //deleteAllergy(allergyId: string): Promise<Result<void>>;

}