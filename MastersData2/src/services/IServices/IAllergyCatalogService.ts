import { Result } from "../../core/logic/Result";
import IAllergyCatalogItemDTO from "../../dto/IAllergyCatalogItemDTO";


export default interface IAllergyCatalogService {
    createAllergyCatalogItem(allergyDTO: IAllergyCatalogItemDTO): Promise<Result<IAllergyCatalogItemDTO>>;
    listAllergiesCatalogItems(): Promise<Result<IAllergyCatalogItemDTO[]>>;
    updateAllergyCatalogItem(name: string, nameToEdit: string): Promise<Result<IAllergyCatalogItemDTO>>;

}