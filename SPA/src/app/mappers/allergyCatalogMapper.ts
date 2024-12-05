import { AllergyCatalogItem } from "../models/allergyCatalog";



export class AllergyCatalogMapper {

    static mapToAllergyCatalogItem(data: any): AllergyCatalogItem {
        return {
            id: data.id,
            name: data.name
        };
    }


    static mapToAllergyCatalogItems(data: any[]): AllergyCatalogItem[] {
        return data.map(item => this.mapToAllergyCatalogItem(item));
    }




}