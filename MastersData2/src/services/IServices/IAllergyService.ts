import { Result } from "../../core/logic/Result";
import IAllergyDTO from "../../dto/IAllergyDTO";


export default interface IAllergyService {
    createAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>>;
    listAllergies(): Promise<Result<IAllergyDTO[]>>;
    //updateAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>>;
    //deleteAllergy(allergyId: string): Promise<Result<void>>;

}