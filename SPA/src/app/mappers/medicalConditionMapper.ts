import { MedicalCondition } from "../models/medicalCondition";



export class AllergyCatalogMapper {

    static mapToMedicalCondition(data: any): MedicalCondition {
        return {
            id: data.id,
            name: data.name
        };
    }


    static mapToMedicalConditions(data: any[]): MedicalCondition[] {
        return data.map(item => this.mapToMedicalCondition(item));
    }




}