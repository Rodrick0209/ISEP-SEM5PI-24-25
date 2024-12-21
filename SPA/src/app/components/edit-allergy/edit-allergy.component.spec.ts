import { ComponentFixture, TestBed } from '@angular/core/testing';
import { EditAllergyComponent } from './edit-allergy.component';
import { AllergyCatalogService } from '../../services/allergiesCatalog.service';
import { ActivatedRoute, Router } from '@angular/router';
import { MessageService } from '../../services/message.service';
import { of, throwError } from 'rxjs';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

describe('EditAllergyComponent', () => {
  let component: EditAllergyComponent;
  let fixture: ComponentFixture<EditAllergyComponent>;
  let mockAllergyService: jasmine.SpyObj<AllergyCatalogService>;
  let mockRouter: jasmine.SpyObj<Router>;
  let mockActivatedRoute: any;
  let mockMessageService: jasmine.SpyObj<MessageService>;

  beforeEach(async () => {
    mockAllergyService = jasmine.createSpyObj('AllergyCatalogService', ['getAllergyCatalogItem', 'updateAllergyCatalogItem']);
    mockAllergyService.getAllergyCatalogItem.and.returnValue(of({ id: 'id1', code: '1', designation: 'Peanuts', description: 'Allergy to peanuts' }));
    mockAllergyService.updateAllergyCatalogItem.and.returnValue(of({ id: 'id1', code: '1', designation:'Updated Allergy', description: 'Allergy to peanuts' }));
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);
    mockMessageService = jasmine.createSpyObj('MessageService', ['setMessage']);
    mockActivatedRoute = {
      snapshot: {
        paramMap: {
          get: jasmine.createSpy('get').and.returnValue('id1')
        }
      }
    };

    await TestBed.configureTestingModule({
      imports: [FormsModule, CommonModule],
      providers: [
        { provide: AllergyCatalogService, useValue: mockAllergyService },
        { provide: Router, useValue: mockRouter },
        { provide: ActivatedRoute, useValue: mockActivatedRoute },
        { provide: MessageService, useValue: mockMessageService }
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(EditAllergyComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize with allergy name from route', () => {
    mockAllergyService.getAllergyCatalogItem.and.returnValue(of({ id: 'id1', code: '1', designation: 'Peanuts', description: 'Allergy to peanuts' }));
    component.ngOnInit();
    expect(component.allergyName).toBe('id1');
    expect(component.submitForm.designation).toBe('Peanuts');
    expect(component.submitForm.description).toBe('Allergy to peanuts');
  });

  it('should show confirmation modal on confirmSubmission', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal on closeConfirmationModal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  it('should update allergy on valid submit', () => {
    mockAllergyService.updateAllergyCatalogItem.and.returnValue(of({ id: 'id1', code: '1', designation:'Updated Allergy', description: 'Allergy to peanuts' }));
    component.allergyName = 'id1';
    component.submitForm.designation = 'Updated Allergy';
    component.submitForm.description = '';
    component.onSubmit({});
    expect(mockAllergyService.updateAllergyCatalogItem).toHaveBeenCalledWith('id1', 'Updated Allergy', '');
    expect(mockMessageService.setMessage).toHaveBeenCalledWith('Allergy id1 successfully edited!');
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/allergiesCatalog']);
  });

  it('should navigate to allergies catalog on cancel', () => {
    component.onCancel();
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/allergiesCatalog']);
  });
});