import { Entity } from "../core/domain/Entity";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { AggregateRoot } from "../core/domain/AggregateRoot";

import IMedicalConditionCatalogDTO from "../dto/IMedicalConditionCatalogDTO";


interface MedicalConditionCatalogProps {
    name: string;
}


export class MedicalConditionCatalog extends AggregateRoot<MedicalConditionCatalogProps> {
    get id() : UniqueEntityID {
        return this._id;
    }

    get medicalConditionCatalogId (): UniqueEntityID {
        return this.id;
    }

    get name (): string {
        return this.props.name;
    }

    set name (name: string) {
        this.props.name = name;
    }

    private constructor (props: MedicalConditionCatalogProps, id?: UniqueEntityID) {
        super(props, id);
    }


    public static create (medicalConditionDTO: IMedicalConditionCatalogDTO, id?: UniqueEntityID): Result<MedicalConditionCatalog> {
        const name = medicalConditionDTO.name;

        if (!!name === false || name.length === 0) {
            return Result.fail<MedicalConditionCatalog>('Must provide a name for the medical condition')
        } else {
            const medicalCondition = new MedicalConditionCatalog({ name: name }, id);
            return Result.ok<MedicalConditionCatalog>( medicalCondition )
        }
    }
}