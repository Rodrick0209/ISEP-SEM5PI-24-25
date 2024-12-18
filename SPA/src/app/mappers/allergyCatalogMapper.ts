import { AllergyCatalogItem } from "../models/allergyCatalog";



export class AllergyCatalogMapper {

    static mapToAllergyCatalogItem(data: any): AllergyCatalogItem {
        return {
            id: data.id,
            code: data.code,
            designation: data.designation,
            description: data.description
        };
    }


    static mapToAllergyCatalogItems(data: any[]): AllergyCatalogItem[] {
        return data.map(item => this.mapToAllergyCatalogItem(item));
    }




}