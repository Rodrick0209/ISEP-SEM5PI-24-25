import { Entity } from "../core/domain/Entity";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { AggregateRoot } from "../core/domain/AggregateRoot";

import IMedicalConditionDTO from "../dto/IMedicalConditionDTO";


interface MedicalConditionProps {
    name: string;
}


export class MedicalCondition extends AggregateRoot<MedicalConditionProps> {
    get id() : UniqueEntityID {
        return this._id;
    }

    get medicalConditionId (): UniqueEntityID {
        return this.id;
    }

    get name (): string {
        return this.props.name;
    }

    private constructor (props: MedicalConditionProps, id?: UniqueEntityID) {
        super(props, id);
    }


    public static create (medicalConditionDTO: IMedicalConditionDTO, id?: UniqueEntityID): Result<MedicalCondition> {
        const name = medicalConditionDTO.name;

        if (!!name === false || name.length === 0) {
            return Result.fail<MedicalCondition>('Must provide a name for the medical condition')
        } else {
            const medicalCondition = new MedicalCondition({ name: name }, id);
            return Result.ok<MedicalCondition>( medicalCondition )
        }
    }
}