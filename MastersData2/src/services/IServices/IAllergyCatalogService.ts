import { Result } from "../../core/logic/Result";
import IAllergyCatalogItemDTO from "../../dto/IAllergyCatalogItemDTO";


export default interface IAllergyCatalogService {
    createAllergyCatalogItem(allergyDTO: IAllergyCatalogItemDTO): Promise<Result<IAllergyCatalogItemDTO>>;
    listAllergiesCatalogItems(): Promise<Result<IAllergyCatalogItemDTO[]>>;
    //updateAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>>;
    //deleteAllergy(allergyId: string): Promise<Result<void>>;

}