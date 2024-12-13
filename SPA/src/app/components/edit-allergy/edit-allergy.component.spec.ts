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
    mockAllergyService.getAllergyCatalogItem.and.returnValue(of({ id: '1', name: 'Peanuts' }));
    mockAllergyService.updateAllergyCatalogItem.and.returnValue(of({ id: '1', name: 'Updated Allergy' }));
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);
    mockMessageService = jasmine.createSpyObj('MessageService', ['setMessage']);
    mockActivatedRoute = {
      snapshot: {
        paramMap: {
          get: jasmine.createSpy('get').and.returnValue('testAllergy')
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
    mockAllergyService.getAllergyCatalogItem.and.returnValue(of({ id: '1', name: 'Peanuts' }));
    component.ngOnInit();
    expect(component.allergyName).toBe('testAllergy');
    expect(component.submitForm.name).toBe('Peanuts');
  });

  it('should handle error when loading allergy', () => {
    spyOn(console, 'error');
    mockAllergyService.getAllergyCatalogItem.and.returnValue(throwError({ error: 'Error loading allergy' }));
    component.ngOnInit();
    expect(console.error).toHaveBeenCalledWith('Error loading allergy', { error: 'Error loading allergy' });
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
    mockAllergyService.updateAllergyCatalogItem.and.returnValue(of({id: '1', name: 'Updated Allergy'}));
    component.allergyName = 'testAllergy';
    component.submitForm.name = 'Updated Allergy';
    component.onSubmit({});
    expect(mockAllergyService.updateAllergyCatalogItem).toHaveBeenCalledWith('testAllergy', 'Updated Allergy');
    expect(mockMessageService.setMessage).toHaveBeenCalledWith('Allergy testAllergy successfully edited!');
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/allergiesCatalog']);
  });

  it('should handle error on failed submit', () => {
    spyOn(console, 'error');
    mockAllergyService.updateAllergyCatalogItem.and.returnValue(throwError({ error: { message: 'Failed to edit allergy' } }));
    component.allergyName = 'testAllergy';
    component.submitForm.name = 'Updated Allergy';
    component.onSubmit({});
    expect(component.errorMessage).toBe('Failed to edit allergy');
    expect(console.error).toHaveBeenCalledWith('Failed to edit allergy', { error: { message: 'Failed to edit allergy' } });
  });

  it('should navigate to allergies catalog on cancel', () => {
    component.onCancel();
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/allergiesCatalog']);
  });
});