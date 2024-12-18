import { Entity } from "../core/domain/Entity";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { AggregateRoot } from "../core/domain/AggregateRoot";

import IMedicalConditionCatalogDTO from "../dto/IMedicalConditionCatalogDTO";


interface MedicalConditionCatalogProps {
    code: string;
    designation: string;
    description?: string;
    commonSymptoms?: string[];
}


export class MedicalConditionCatalog extends AggregateRoot<MedicalConditionCatalogProps> {
    get id(): UniqueEntityID {
        return this._id;
    }

    get medicalConditionCatalogId(): UniqueEntityID {
        return this.id;
    }

    get code(): string {
        return this.props.code;
    }

    get designation(): string {
        return this.props.designation;
    }

    get description(): string | null {
        return this.props.description;
    }

    get commonSymptoms(): string[] | null {
        return this.props.commonSymptoms;
    }

    set description(description: string) {
        this.props.description = description;
    }

    set designation(designation: string) {
        this.props.designation = designation;
    }

    private constructor(props: MedicalConditionCatalogProps, id?: UniqueEntityID) {
        super(props, id);
    }


    public static create(medicalConditionDTO: IMedicalConditionCatalogDTO, id?: UniqueEntityID): Result<MedicalConditionCatalog> {
        const code = medicalConditionDTO.code;
        const designation = medicalConditionDTO.designation;
        const description = medicalConditionDTO.description ?? null;
        const commonSymptoms = medicalConditionDTO.commonSymptoms ?? null;

        if (!this.isValidCode(code)) {
            return Result.fail<MedicalConditionCatalog>('Invalid code for the medical condition')
        }

        if (description != null && description.trim().length > 2048) {
            return Result.fail<MedicalConditionCatalog>('Description cannot exceed 2048 characters')
        }

        const medicalConditionCatalog = new MedicalConditionCatalog({ 
            code: code, 
            designation: designation, 
            description: description, 
            commonSymptoms: commonSymptoms }, id);
        return Result.ok<MedicalConditionCatalog>(medicalConditionCatalog);
    }

    private static isValidCode(code: string): boolean {
        const snomedRegex = /^[0-9]{6,}$/; // Example regex for SNOMED CT code
        const icd11Regex = /^[A-Z]{1}[0-9]{2}(\.[A-Z0-9]{1,4})?$/; // Example regex for ICD-11 code

        return snomedRegex.test(code) || icd11Regex.test(code);
    }
}