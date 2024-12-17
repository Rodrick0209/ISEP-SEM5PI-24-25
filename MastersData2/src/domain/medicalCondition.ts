import { Entity } from "../core/domain/Entity";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { AggregateRoot } from "../core/domain/AggregateRoot";
import { MedicalConditionCatalog } from './medicalConditionCatalog';
import { IMedicalConditionDTO } from "../dto/IMedicalConditionDTO";
import { MedicalConditionId } from './medicalConditionId';

interface MedicalConditionProps {
    medicalConditionCatalog: MedicalConditionCatalog;
    date: Date;
}

export class MedicalCondition extends AggregateRoot<MedicalConditionProps> {
    get id() : UniqueEntityID {
        return this._id;
    }

    get MedicalConditionId (): UniqueEntityID {
        return this.id;
    }

    get code (): string {
        return this.props.medicalConditionCatalog.code;
    }

    get designation (): string {
        return this.props.medicalConditionCatalog.designation;
    }

    get date (): Date {
        return this.props.date;
    }

    private constructor (props: MedicalConditionProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create (medicalConditionDTO: MedicalConditionCatalog, dat : Date, id?: UniqueEntityID): Result<MedicalCondition> {
        const medicalConditionCatalog = medicalConditionDTO;
        const date = dat;

        if ( medicalConditionCatalog === null || date === null) {
            return Result.fail<MedicalCondition>('Must provide a valid allergy and date for the allergy')
        } else {
            const allergy = new MedicalCondition({ medicalConditionCatalog: medicalConditionCatalog, date: date }, id);
            return Result.ok<MedicalCondition>( allergy )
        }

    }



}