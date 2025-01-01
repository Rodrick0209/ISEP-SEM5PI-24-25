import { Result } from "../../core/logic/Result";
import IAllergyCatalogItemDTO from "../../dto/IAllergyCatalogItemDTO";


export default interface IAllergyCatalogService {
    createAllergyCatalogItem(allergyDTO: IAllergyCatalogItemDTO): Promise<Result<IAllergyCatalogItemDTO>>;
    listAllergiesCatalogItems(): Promise<Result<IAllergyCatalogItemDTO[]>>;
    updateAllergyCatalogItem(code: string, allergyDTO: IAllergyCatalogItemDTO): Promise<Result<IAllergyCatalogItemDTO>>;
    getAllergyCatalogItem(code: string): Promise<Result<IAllergyCatalogItemDTO>>;
    deleteAllergyCatalogItem(code: string): Promise<Result<void>>;
}