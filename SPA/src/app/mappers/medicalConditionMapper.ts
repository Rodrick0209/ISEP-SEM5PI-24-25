import { MedicalCondition } from "../models/medicalCondition";



export class AllergyCatalogMapper {

    public static mapToMedicalCondition(data: any): MedicalCondition {
        return {
            id: data.id,
            code: data.code,
            designation: data.designation,
            date: new Date(data.date)
        };
    }

    static mapToMedicalConditions(data: any[]): MedicalCondition[] {
        return data.map(item => this.mapToMedicalCondition(item));
    }




}