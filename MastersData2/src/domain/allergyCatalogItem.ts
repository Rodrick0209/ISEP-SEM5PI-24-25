import { Entity } from "../core/domain/Entity";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import IAllergyCatalogItemDTO from "../dto/IAllergyCatalogItemDTO";
import { AggregateRoot } from "../core/domain/AggregateRoot";



interface AllergyCatalogItemProps {
    code: string;
    designation: string;
    description?: string;
}


export class AllergyCatalogItem extends AggregateRoot<AllergyCatalogItemProps> {
    get id() : UniqueEntityID {
        return this._id;
    }

    get allergyId (): UniqueEntityID {
        return this.id;
    }

    get code (): string {
        return this.props.code;
    }

    get designation (): string {
        return this.props.designation;
    }

    set designation (name: string) {
        this.props.designation = name;
    }
    
    get description (): string | null {
        return this.props.description ;
    }

    set description (description: string) {
        this.props.description = description;
    }

    private constructor (props: AllergyCatalogItemProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create (allergyCatalogDTO: IAllergyCatalogItemDTO, id?: UniqueEntityID): Result<AllergyCatalogItem> {
        const code = allergyCatalogDTO.code;
        const designation = allergyCatalogDTO.designation;
        const description = allergyCatalogDTO.description ?? null;

        const allergyCatalogItem = new AllergyCatalogItem({ 
            code: code, 
            designation: designation, 
            description: description
        }, id);
        return Result.ok<AllergyCatalogItem>( allergyCatalogItem )
    }

    private static isValidCode(code: string): boolean {
        const snomedRegex = /^[0-9]{6,}$/; // Example regex for SNOMED CT code
        const icd11Regex = /^[A-Z]{1}[0-9]{2}(\.[A-Z0-9]{1,4})?$/; // Example regex for ICD-11 code

        return snomedRegex.test(code) || icd11Regex.test(code);
    }
}